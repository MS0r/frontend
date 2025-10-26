import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest'
import { mount, flushPromises } from '@vue/test-utils'
import { createPinia, setActivePinia } from 'pinia'
import LoginView from '@/views/LoginView.vue'
import RegisterView from '@/views/RegisterView.vue'
import AskQuestionView from '@/views/AskQuestionView.vue'
import SubunitView from '@/views/SubunitView.vue'
import ForumQuestionView from '@/views/ForumQuestionView.vue'
import ForumHomeView from '@/views/ForumHomeView.vue'
import router from '@/router'

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


describe("Views Tests", () => {
  it('Authentication flow: LoginView stores token and redirects on success', async () => {
    // stub fetch to return token
    global.fetch = vi.fn(() => Promise.resolve({
      ok: true,
      json: () => Promise.resolve({ token: 'tok_123' })
    }))

    const wrapper = mount(LoginView, {
      global: {
        plugins: [router]
      }
    })

    // Fill inputs (find by placeholder)
    await wrapper.find('input[type="email"]').setValue('a@b.com')
    await wrapper.find('input[type="password"]').setValue('pwd')

    // submit form
    await wrapper.find('form').trigger('submit.prevent')
    await flushPromises()

    expect(global.fetch).toHaveBeenCalledWith(`${API_VITE_URL}/auth/login`, expect.objectContaining({
      method: 'POST'
    }))
    expect(localStorage.getItem('token')).toBe('tok_123')
    expect(router.push).toHaveBeenCalled()
  })


  it('RegisterView: register posts to /api/auth/register and redirects on success', async () => {
    // Note: RegisterView.register calls fetch synchronously (no await).
    // Return a synchronous object with ok:true so register() sees res.ok === true.
    const API_VITE_URL = import.meta.env.VITE_API_URL
    global.fetch = vi.fn(() => ({ ok: true }))
    const wrapper = mount(RegisterView, {
      global: {
        plugins: [router]
      }
    })

    await wrapper.find('input[placeholder="Nombre"]').setValue('Test User')
    await wrapper.find('input[placeholder="Correo electrónico"]').setValue('a@b.com')
    await wrapper.find('input[placeholder="Contraseña"]').setValue('pwd')

    await wrapper.find('form').trigger('submit.prevent')

    expect(global.fetch).toHaveBeenCalledWith(`${API_VITE_URL}/auth/register`, expect.objectContaining({
      method: 'POST',
      headers: { 'Content-Type': 'application/json' }
    }))
    expect(router.push).toHaveBeenCalledWith('/login')
  })

  it('Form submissions & validation: AskQuestionView submits payload with Authorization header and navigates back', async () => {
    localStorage.setItem('token', 'tok_abc')
    const API_VITE_URL = import.meta.env.VITE_API_URL

    global.fetch = vi.fn(() => Promise.resolve({ ok: true }))

    const wrapper = mount(AskQuestionView, {
      global: { plugins: [router] }
    })

    await wrapper.find('input').setValue('My Title')
    await wrapper.findAll('textarea')[0].setValue('Body content')
    await wrapper.findAll('input')[1].setValue('tag1,tag2')

    await wrapper.find('form').trigger('submit.prevent')
    await flushPromises()

    expect(global.fetch).toHaveBeenCalledWith(`${API_VITE_URL}/forum/questions`, expect.objectContaining({
      method: 'POST',
      headers: expect.objectContaining({ 'Authorization': 'Token tok_abc' })
    }))

    expect(router.push).toHaveBeenCalledWith('/forum')
  })

  it('Subunit: quiz submit posts success when all answers correct', async () => {
    const subunit = {
      title: 'Quiz Subunit',
      blocks: [],
      order: 1,
      quiz: {
        id: 99,
        title: 'Quiz',
        questions: [
          {
            question_text: 'Q1',
            options: [
              { text: 'A', is_correct: true },
              { text: 'B', is_correct: false }
            ]
          }
        ]
      }
    }

    global.fetch = vi.fn(() => Promise.resolve({ ok: true }))
    localStorage.setItem('token', 'tok_xyz')

    const wrapper = mount(SubunitView, {
      props: { subunit },
      global: { plugins: [router] }
    })

    // select the correct option (radio)
    const radio = wrapper.find('input[type="radio"]')
    await radio.setValue('0')

    // click submit
    await wrapper.find('button.quiz-submit').trigger('click')
    await flushPromises()

    expect(global.fetch).toHaveBeenCalled()
    expect(wrapper.text()).toContain('You got 1 out of 1 correct.')
  })



  it('ForumQuestionView: fetches question then voting updates counts', async () => {
    const question = {
      "id": 2,
      "title": "How to define a module in Erlang?",
      "body": "What is the syntax to define a module?",
      "tags": [
        "erlang",
        "module"
      ],
      "user_id": 2,
      "views": 0,
      "answer_count": null,
      "upvote_count": 1,
      "downvote_count": 0,
      "answers": [
        {
          "id": 2,
          "body": "Use -module(module_name). at the top of your file.",
          "question_id": 2,
          "user_id": 1,
          "upvote_count": 0,
          "downvote_count": 0,
          "user": "user1"
        }
      ],
      "user": "user2"
    }

    const answer = {
      "id": 3,
      "body": "Answer",
      "question_id": 2,
      "user_id": 1,
      "upvote_count": 0,
      "downvote_count": 0,
      "user": "user1"
    }

    const votes = [
      { id: 1, vote: 'upvote', question_id: 2 },
      { id: 2, vote: 'downvote', answer_id: 2 },
      {id: 3, vote: 'upvote', answer_id: 2 }
    ]


    global.fetch = vi.fn((url, opts) => {
      const body = opts?.body ? JSON.parse(opts.body) : null
      if (url.includes(`${API_VITE_URL}/forum/questions/view`)) {

        return Promise.resolve({
          ok: true,
          json: () => Promise.resolve(question)
        })

      } else if (url.includes(`${API_VITE_URL}/forum/vote/answer`)) {

        if (body?.vote === "upvote") {
          question.answers[0].upvote_count += 1
        } else if (body?.vote === "downvote") {
          question.answers[0].downvote_count += 1
        }
        return Promise.resolve({
          ok: true,
          json: () => Promise.resolve(question.answers[0])
        })

      } else if (url.includes(`${API_VITE_URL}/forum/vote`) && opts?.method === 'POST') {

        if (body?.vote === "upvote") {
          question.upvote_count += 1
        } else if (body?.vote === "downvote") {
          question.downvote_count += 1
        }
        return Promise.resolve({
          ok: true,
          json: () => Promise.resolve(question)
        })

      } else if (url.includes(`${API_VITE_URL}/forum/vote`)){
        return Promise.resolve({
          ok: true,
          json: () => Promise.resolve(votes)
        })
      } else if (url.includes(`${API_VITE_URL}/forum/questions`) && opts?.method === 'POST') {

        return Promise.resolve({
          ok: true,
          json: () => Promise.resolve(answer)
        })

      }
    })

    localStorage.setItem('token', 'tok_123')
    const wrapper = mount(ForumQuestionView, {
      global: { plugins: [router] }
    })

    await flushPromises()
    expect(wrapper.text()).toContain('Erlang')

    const upBtn = wrapper.find("#upvote-q")
    await upBtn.trigger('click')
    await flushPromises()
    expect(wrapper.vm.question.upvote_count).toBe(2)

    const backBtn = wrapper.find('button.back-btn')
    await backBtn.trigger('click')
    expect(router.push).toHaveBeenCalled()

    const voteAnswer = wrapper.find("#upvote-a-0")
    await voteAnswer.trigger('click')
    await flushPromises()

    expect(wrapper.vm.question.answers[0].upvote_count).toBe(1)

    const textarea = wrapper.find("textarea")
    await textarea.setValue("Answer")
    const submitAnswer = wrapper.find("form")
    await submitAnswer.trigger("submit.prevent")
    await flushPromises()

    expect(wrapper.vm.question.answers.length).toBe(2)
    expect(wrapper.vm.question.answers[1].body).toBe("Answer")
  })

  it('ForumHomeView renders list of questions when given via props', async () => {
    const questions = [
      { id: 1, title: 'First', body: 'b', upvote_count: 0, downvote_count: 0, answer_count: 0, tags: [] },
      { id: 2, title: 'Second', body: 'b2', upvote_count: 1, downvote_count: 0, answer_count: 2, tags: ['t'] }
    ]

    const wrapper = mount(ForumHomeView, {
      props: { questions, searchQuery: '' },
      global: { plugins: [router] }
    })

    expect(wrapper.findAll('.question-item').length).toBe(2)
    expect(wrapper.text()).toContain('First')
    expect(wrapper.text()).toContain('Second')
  })

  it('SubunitView.goToSubunit: does nothing if going before first subunit and no previous unit exists', async () => {
    global.fetch = vi.fn(() => Promise.resolve({
      ok: true,
      json: () => Promise.resolve([
        {
          order: 1,
          title: 'Unit 1',
          subunits: [
            { order: 1, title: 'Subunit 1.1', blocks: [] },
            { order: 2, title: 'Subunit 1.2', blocks: [] },
            { order: 3, title: 'Subunit 1.3', blocks: [] },
            { order: 4, title: 'Subunit 1.4', blocks: [] }
          ]
        },
        {
          order: 2,
          title: 'Unit 2',
          subunits: [
            { order: 1, title: 'Subunit 2.1', blocks: [] },
            { order: 2, title: 'Subunit 2.2', blocks: [] }
          ]
        }
      ])
    }))
    vi.mock('vue-router', async (importOriginal) => {
      const actual = await importOriginal();
      return {
        ...actual,
        useRoute: () => ({
          params: { unitOrder: '1', subunitOrder: '3', courseId: '1' },
        }),
      };
    });

    const wrapper = mount(SubunitView, {
      global: {
        plugins: [router]
      },
      props: { subunit: { title: "Test Subunit", blocks: [] } }
    })

    await wrapper.vm.goToSubunit(-3)
    expect(router.push).not.toHaveBeenCalled()

    await wrapper.vm.goToSubunit(-1)
    expect(router.push).toHaveBeenCalledWith('/course/1/unit/1/subunit/2')

    await wrapper.vm.goToSubunit(2)
    expect(router.push).toHaveBeenCalledWith('/course/1/unit/1/exercise/')
  })
})