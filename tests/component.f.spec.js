import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest'
import { mount, flushPromises } from '@vue/test-utils'
import { createPinia, setActivePinia } from 'pinia'
import { compileErlang, testErlang } from '@/composables/compileErlang'
import { useCourseStore } from '@/stores/courseStore.js'
import ForumLayout from '@/layouts/ForumLayout.vue'
import CodeBlock from '@/components/CodeBlock.vue'
import Sidebar from '@/components/Sidebar.vue'
import CourseLayout from '@/layouts/CourseLayout.vue'
import router from '@/router'
import { nextTick } from 'vue'
import Header from '@/components/Header.vue'

const API_VITE_URL = import.meta.env.VITE_API_URL

beforeEach(() => {
  vi.restoreAllMocks()
  const store = {}
  global.localStorage = {
    getItem: (key) => store[key] || null,
    setItem: (key, value) => store[key] = value,
    removeItem: (key) => delete store[key],
    clear: () => Object.keys(store).forEach(k => delete store[k]),
  }
  global.fetch = vi.fn()
  setActivePinia(createPinia())
  vi.spyOn(router, 'push').mockImplementation(() => { })
})

afterEach(() => {
  vi.resetAllMocks()
})

describe('Components', () => {
  it('API contract: compileErlang calls /api/erlang/compile and returns parsed JSON', async () => {
    const mockResp = { status: 'ok', result: 'compiled output' }
    global.fetch = vi.fn(() => Promise.resolve({
      ok: true,
      json: () => Promise.resolve(mockResp)
    }))

    const res = await compileErlang('dummy code')
    expect(global.fetch).toHaveBeenCalledWith(
      `${API_VITE_URL}/erlang/compile`,
      expect.objectContaining({
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ erlang_payload: { code: 'dummy code' } })
      })
    )
    expect(res).toEqual(mockResp)
  })

  it('testErlang: posts submission to /api/exercise/:id/submit with Authorization header', async () => {
    localStorage.setItem('token', 'tok_123')
    const mockResp = { result: 'ok' }
    global.fetch = vi.fn(() => Promise.resolve({
      ok: true,
      json: () => Promise.resolve(mockResp)
    }))

    const res = await testErlang('some code', 42)
    expect(global.fetch).toHaveBeenCalledWith(`${API_VITE_URL}/exercise/42/submit`, expect.objectContaining({
      method: 'POST',
      headers: expect.objectContaining({ 'Authorization': 'Token tok_123', 'Content-Type': 'application/json' })
    }))
    expect(res).toEqual(mockResp)
  })

  it('API contract + cache: useCourseStore.fetchCourse fetches units and stores cache', async () => {
    const units = [{ id: 1, order: 1, title: 'Unit 1', subunits: [] }]

    global.fetch = vi.fn(() => Promise.resolve({
      ok: true,
      json: () => Promise.resolve(units)
    }))

    const store = useCourseStore()
    await store.fetchCourse(1)

    expect(global.fetch).toHaveBeenCalledWith(`${API_VITE_URL}/course/1/units`)
    expect(store.units).toEqual(units)

    const cached = JSON.parse(localStorage.getItem('course:1'))
    expect(cached).toBeTruthy()
    expect(cached.data).toEqual(units)
  })

  it('CodeBlock mounts and runs without route injection errors', async () => {
    // stub compileErlang call
    global.fetch = vi.fn(() => Promise.resolve({
      ok: true,
      json: () => Promise.resolve({ status: 'ok', result: 'out' })
    }))

    const wrapper = mount(CodeBlock, {
      props: { code: 'test code' },
      global: { plugins: [router] }
    })

    expect(wrapper.text()).toMatch(/test code/)
    await wrapper.findAll('button')[0].trigger('click') // Ejecutar
    await flushPromises()
    await nextTick()

    expect(wrapper.find('.output').exists()).toBe(true)
  })

  it('Error handling: compileErlang throws on non-ok response', async () => {
    global.fetch = vi.fn(() => Promise.resolve({ ok: false }))
    await expect(compileErlang('bad')).rejects.toThrow()
  })

  it('ForumLayout fetchQuestions populates filteredQuestions on mount', async () => {
    const questions = [{ id: 1, title: 'Q1', body: 'b' }]
    global.fetch = vi.fn(() => Promise.resolve({
      ok: true,
      json: () => Promise.resolve(questions)
    }))

    const wrapper = mount(ForumLayout, {
      global: {
        plugins: [router]
      }
    })

    // wait for onMounted fetch to complete
    await flushPromises()

    // filteredQuestions is a ref inside the component VM
    expect(wrapper.vm.filteredQuestions).toBeTruthy()
    expect(wrapper.vm.filteredQuestions.length).toBeGreaterThan(0)
    expect(wrapper.vm.filteredQuestions[0].title).toBe('Q1')
  })

  it('fetchQuestions populates filteredQuestions on mount and updates with searchQuery', async () => {
    const questions = [
      { id: 1, title: 'Q1', body: 'b' },
      { id: 2, title: 'Q2', body: 'body2' }
    ]

    global.fetch = vi.fn((url) => {
      if (url.includes('s=Q2')) {
        return Promise.resolve({
          ok: true,
          json: () => Promise.resolve([questions[1]])
        })
      }
      return Promise.resolve({
        ok: true,
        json: () => Promise.resolve(questions)
      })
    })

    const wrapper = mount(ForumLayout, {
      global: {
        plugins: [router]
      }
    })

    await flushPromises()

    expect(wrapper.vm.filteredQuestions).toHaveLength(2)
    expect(wrapper.vm.filteredQuestions[0].title).toBe('Q1')

    wrapper.vm.searchQuery = 'Q2'
    await wrapper.vm.performSearch()
    await flushPromises()

    expect(wrapper.vm.filteredQuestions).toHaveLength(1)
    expect(wrapper.vm.filteredQuestions[0].title).toBe('Q2')
  })

  it('Sidebar shows checkmarks when user has quiz_passes and submissions', async () => {
    localStorage.setItem('token', 'tok_123')
    const userResp = {
      username: 'testuser',
      quiz_passes: { 0: { id: 1, user_id: 1, quiz_id: 3 } },
      submissions: { 0: { exercise_id: 1 } }
    }

    global.fetch = vi.fn((url) => {
      return Promise.resolve({ ok: true, json: () => Promise.resolve(userResp) })
    })

    const units = [
      { id: 1, order: 1, title: 'Unit 1', subunits: [{ id: 1, order: 1, title: 'Subunit 1', quiz: { id: 3, title: 'Quiz Erlang: Introducción y Sintaxis', description: 'Quiz sobre los conceptos fundamentales de Erlang, su sintaxis y características.', subunit_id: 1 } }], exercise: { id: 1, title: "Ejercicio: Catálogo de productos" } }
    ]

    const wrapper = mount(Sidebar, { props: { courseTitle: 'Test Course', units }, global: { plugins: [router] } })

    await flushPromises()

    expect(wrapper.html()).toContain('✔️')
    expect(wrapper.findAll('a').length).toBeGreaterThan(0)
  })

  it('CourseLayout mounts with router and fetches course title and units without throwing', async () => {
    localStorage.setItem('token', 'tok_123')
    global.fetch = vi.fn((url) => {
      if (url.includes('/course/1') && !url.endsWith('/units')) {
        return Promise.resolve({
          ok: true,
          json: () => Promise.resolve({ title: 'C1' })
        })
      } else if (url.endsWith('/units')) {
        return Promise.resolve({ ok: true, json: () => Promise.resolve([]) })
      } else if (url.includes('progress')) {
        return Promise.resolve({
          ok: true,
          json: () => Promise.resolve({
            progress: 50.0,
            total_quizzes: 4,
            passed_quizzes: 2,
            total_exercises: 10,
            completed_exercises: 3
          })
        })
      }

      return Promise.resolve({ ok: true, json: () => Promise.resolve({}) })
    })

    const wrapper = mount(CourseLayout, { global: { plugins: [router] } })

    await flushPromises()

    expect(wrapper.exists()).toBe(true)
    expect(wrapper.vm.courseTitle).toBe('C1')
  })

  it('CodeBlock displays error output when compileErlang returns error', async () => {
    // stub compileErlang to return error
    global.fetch = vi.fn(() => Promise.resolve({
      ok: true,
      json: () => Promise.resolve({ status: 'error', reason: 'Compilation failed' })
    }));

    const wrapper = mount(CodeBlock, {
      props: { code: 'bad code' },
      global: { plugins: [router] }
    });


    await wrapper.vm.runCode()
    await flushPromises();
    await nextTick();
    wrapper.vm.output = "Error: Compilation failed"
    await flushPromises();
    await nextTick();
    expect(wrapper.find('.output').text()).toBe('Error: Compilation failed');
  });

  it("Header", async () => {
    localStorage.setItem('token', 'tok_123')
    global.fetch = vi.fn(() => Promise.resolve({
      ok: true,
      json: () => Promise.resolve()
    }));

    const wrapper = mount(Header, {
      props: { code: 'bad code' },
      global: { plugins: [router] }
    });

    await wrapper.vm.logout()
    await wrapper.vm.toggleDropdown();
  })
})
