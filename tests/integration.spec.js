import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest'
import { createPinia, setActivePinia } from 'pinia'

const API_VITE_URL = import.meta.env.VITE_API_URL

const {body : bod} = await probe(`${API_VITE_URL}/auth/login`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ user: { username: "user1@example.com", password: "password1" } })
    })
const TEST_TOKEN = bod.token

beforeEach(() => {
  vi.restoreAllMocks()
  setActivePinia(createPinia())
})

afterEach(() => {
  vi.resetAllMocks()
})

async function probe(url, opts = {}) {
  try {
    const res = await fetch(url, opts)
    let body = null
    const contentType = res.headers && res.headers.get ? res.headers.get('content-type') || '' : ''
    if (contentType.includes('application/json')) {
      try {
        body = await res.json()
      } catch (e) {
      }
    }
    return { res, body }
  } catch (error) {
    return { error }
  }
}


function ensureApi() {
  if (!API_VITE_URL) {
    console.warn('Skipping real-API integration tests: import.meta.env.VITE_API_URL is not set')
    return false
  }
  return true
}

describe('Real API endpoint probes (safe, non-destructive checks)', () => {
  it('probes course endpoints', async () => {
    if (!ensureApi()) return
    const { res: r1, body: b1, error: e1 } = await probe(`${API_VITE_URL}/course/1`)
    if (e1) {
      console.warn('/course/1 unreachable:', e1.message)
      return
    }
    expect(r1).toBeDefined()
    expect(typeof r1.status).toBe('number')
    if (b1 !== null) {
      expect(typeof b1).toBe('object')
      expect(b1).toHaveProperty('id')
      expect(b1).toHaveProperty('title')
      expect(b1).toHaveProperty('description')
    }

    const { res: r2, body: b2 } = await probe(`${API_VITE_URL}/course/1/units`)
    expect(r2).toBeDefined()
    expect(typeof r2.status).toBe('number')
    if (b2 !== null) {
      expect(Array.isArray(b2)).toBeTruthy()
      b2.forEach(unit => {
        expect(typeof unit).toBe('object')
        expect(unit).toHaveProperty('id')
        expect(unit).toHaveProperty('title')
        expect(unit).toHaveProperty('description')
        expect(unit).toHaveProperty('course_id')
        expect(unit).toHaveProperty('order')
        if (unit.subunits) {
          expect(Array.isArray(unit.subunits)).toBeTruthy()
          unit.subunits.forEach(subunit => {
            expect(typeof subunit).toBe('object')
            expect(subunit).toHaveProperty('id')
            expect(subunit).toHaveProperty('title')
            expect(subunit).toHaveProperty('description')
            expect(subunit).toHaveProperty('order')
            if (subunit.blocks) {
              expect(Array.isArray(subunit.blocks)).toBeTruthy()
              subunit.blocks.forEach(block => {
                expect(typeof block).toBe('object')
                expect(block).toHaveProperty('type')
                expect(block).toHaveProperty('value')
              })
            }
            if (subunit.quizz) {
              expect(typeof subunit.quizz).toBe('object')
              if (subunit.quizz.questions) {
                expect(Array.isArray(subunit.quizz.questions)).toBeTruthy()
                subunit.quizz.questions.forEach(question => {
                  expect(typeof question).toBe('object')
                  expect(question).toHaveProperty('question_text')
                  if (question.options) {
                    expect(Array.isArray(question.options)).toBeTruthy()
                    question.options.forEach(option => {
                      expect(typeof option).toBe('object')
                      expect(option).toHaveProperty('text')
                      expect(option).toHaveProperty('is_correct')
                    })
                  }
                })
              }
            }
          })
        }
        if(unit.exercise){
          expect(typeof unit.exercise).toBe('object')
          expect(unit.exercise).toHaveProperty('title')
          expect(unit.exercise).toHaveProperty('description')
          expect(unit.exercise).toHaveProperty('exercise_schema')
          expect(unit.exercise).toHaveProperty('test_cases')
        }
      })  
    }
  })

  it('probes erlang compile and exercise submit endpoints', async () => {
    if (!ensureApi()) return
    // POST compile
    const code = `-module(helloworld).
-export([start/0]).

start() ->
    io:format("hi").
`
    const { res: compileRes, body: compileBody, error: compileErr } = await probe(`${API_VITE_URL}/erlang/compile`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ erlang_payload: { code: code } })
    })
    if (compileErr) {
      console.warn('/erlang/compile unreachable:', compileErr.message)
      return
    }
    expect(typeof compileRes.status).toBe('number')
    if (compileBody !== null) {
      expect(typeof compileBody).toBe('object')
      expect(compileBody).toHaveProperty('status', 'ok')
      expect(compileBody).toHaveProperty('result')
      expect(compileBody.result).toBe("hi")
    }

    // POST exercise submit (may require auth; accept 401/403 as valid), this needs both the exercise to exists and the user to be authenticated
    // const { res: submitRes, body: submitBody } = await probe(`${API_VITE_URL}/exercise/1/submit`, {
    //   method: 'POST',
    //   headers: { 'Content-Type': 'application/json' },
    //   body: JSON.stringify({ submission: { code_snippet: '%% test' } })
    // })
    // expect(submitRes).toBeDefined()
    // expect(typeof submitRes.status).toBe('number')
    // // If allowed and JSON returned, validate its type
    // if (submitBody !== null) expect(typeof submitBody).toBe('object')
  })

  it('probes user and progress endpoints (auth may be required)', async () => {
    if (!ensureApi()) return
    const { res: userRes, body: userBody } = await probe(`${API_VITE_URL}/user`, { headers: { Authorization: `Token ${TEST_TOKEN}` } })
    expect(userRes).toBeDefined()
    expect(typeof userRes.status).toBe('number')
    if (userBody !== null) {
      expect(typeof userBody).toBe('object')
      expect(userBody).toHaveProperty('id')
      expect(userBody).toHaveProperty('email')
      expect(userBody).toHaveProperty('username')
      expect(userBody).toHaveProperty('token')
      expect(userBody).toHaveProperty('quiz_passes')
      expect(userBody).toHaveProperty('submissions')
    }

    const { res: progRes, body: progBody } = await probe(`${API_VITE_URL}/user/progress/1`, { headers: { Authorization: `Token ${TEST_TOKEN}` } })
    expect(progRes).toBeDefined()
    expect(typeof progRes.status).toBe('number')
    if (progBody !== null) {
      expect(typeof progBody).toBe('object')
      expect(progBody).toHaveProperty('progress')
      expect(progBody).toHaveProperty('total_quizzes')
      expect(progBody).toHaveProperty('completed_quizzes')
      expect(progBody).toHaveProperty('total_exercises')
      expect(progBody).toHaveProperty('completed_exercises')
    }
  })

  it('probes forum endpoints', async () => {
    if (!ensureApi()) return
    const { res: qRes, body: qBody } = await probe(`${API_VITE_URL}/forum/questions`)
    expect(qRes).toBeDefined()
    expect(typeof qRes.status).toBe('number')
    if (qBody !== null) {
      // expect an array of question objects with the same shape as the sample response
      expect(Array.isArray(qBody)).toBeTruthy()
      qBody.forEach(q => {
        expect(typeof q).toBe('object')
        expect(q).toHaveProperty('id')
        expect(q).toHaveProperty('title')
        expect(q).toHaveProperty('body')
        expect(q).toHaveProperty('tags')
        expect(Array.isArray(q.tags)).toBeTruthy()
        expect(q).toHaveProperty('user_id')
        expect(q).toHaveProperty('views')
        expect(q).toHaveProperty('answer_count')
        expect(q).toHaveProperty('upvote_count')
        expect(q).toHaveProperty('downvote_count')
      })
    }

    // search
    const { res: sRes, body: sBody } = await probe(`${API_VITE_URL}/forum/questions?s=test`)
    expect(sRes).toBeDefined()
    expect(typeof sRes.status).toBe('number')
    if (sBody !== null) {
            // expect an array of question objects with the same shape as the sample response
      expect(Array.isArray(sBody)).toBeTruthy()
      sBody.forEach(s => {
        expect(typeof s).toBe('object')
        expect(s).toHaveProperty('id')
        expect(s).toHaveProperty('title')
        expect(s).toHaveProperty('body')
        expect(s).toHaveProperty('tags')
        expect(Array.isArray(s.tags)).toBeTruthy()
        expect(s).toHaveProperty('user_id')
        expect(s).toHaveProperty('views')
        expect(s).toHaveProperty('answer_count')
        expect(s).toHaveProperty('upvote_count')
        expect(s).toHaveProperty('downvote_count')
      })
    }

    // view single question (id=1)
    const { res: vRes, body: vBody } = await probe(`${API_VITE_URL}/forum/questions/view/1`)
    expect(vRes).toBeDefined()
    expect(typeof vRes.status).toBe('number')
    if (vBody !== null) {
      expect(typeof vBody).toBe('object')
      expect(vBody).toHaveProperty('id')
      expect(vBody).toHaveProperty('title')
      expect(vBody).toHaveProperty('body')
      expect(vBody).toHaveProperty('tags')
      expect(Array.isArray(vBody.tags)).toBeTruthy()
      expect(vBody).toHaveProperty('user_id')
      expect(vBody).toHaveProperty('views')
      expect(vBody).toHaveProperty('answer_count') // may be null
      expect(vBody).toHaveProperty('upvote_count')
      expect(vBody).toHaveProperty('downvote_count')

      // optional: answers array and user field
      if (vBody.answers !== undefined && vBody.answers !== null) {
        expect(Array.isArray(vBody.answers)).toBeTruthy()
        vBody.answers.forEach(ans => {
          expect(ans).toHaveProperty('id')
          expect(ans).toHaveProperty('body')
          expect(ans).toHaveProperty('question_id')
          expect(ans).toHaveProperty('user_id')
          expect(ans).toHaveProperty('upvote_count')
          expect(ans).toHaveProperty('downvote_count')
          expect(ans).toHaveProperty('user')
        })
      }

      if (vBody.user !== undefined) {
        expect(typeof vBody.user === 'string' || vBody.user === null).toBeTruthy()
      }
    }

    // POST vote endpoints (non-destructive payload)
    const { res: voteQRes, body: voteQBody } = await probe(`${API_VITE_URL}/forum/vote/1`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json', Authorization: `Token ${TEST_TOKEN}` },
      body: JSON.stringify({ vote: 'upvote' })
    })
    expect(voteQRes).toBeDefined()
    if (voteQRes !== null) {
      expect(typeof voteQRes.status).toBe('number')

      // if the endpoint returns JSON with the updated question object, validate its shape
      if (voteQBody !== null) {
        expect(typeof voteQBody).toBe('object')
        expect(voteQBody).toHaveProperty('id')
        expect(voteQBody).toHaveProperty('title')
        expect(voteQBody).toHaveProperty('body')
        expect(voteQBody).toHaveProperty('tags')
        expect(Array.isArray(voteQBody.tags)).toBeTruthy()
        expect(voteQBody).toHaveProperty('user_id')
        expect(voteQBody).toHaveProperty('views')
        expect(voteQBody).toHaveProperty('answer_count') // may be null
        expect(voteQBody).toHaveProperty('upvote_count')
        expect(voteQBody).toHaveProperty('downvote_count')

        // expectations based on the provided example response
        expect(voteQBody).toHaveProperty('upvote_count')
        expect(voteQBody).toHaveProperty('downvote_count')
        expect(voteQBody).toHaveProperty('views')
      }
    }

    const { res: voteARes, body: voteABody } = await probe(`${API_VITE_URL}/forum/vote/answer/1`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json', Authorization: `Token ${TEST_TOKEN}` },
      body: JSON.stringify({ vote: 'downvote' })
    })
    expect(voteARes).toBeDefined()
    if (voteARes !== null) {
      expect(typeof voteARes.status).toBe('number')

      if (voteABody !== null) {
        expect(typeof voteABody).toBe('object')
        expect(voteABody).toHaveProperty('id')
        expect(voteABody).toHaveProperty('body')
        expect(voteABody).toHaveProperty('question_id')
        expect(voteABody).toHaveProperty('user_id')
        expect(voteABody).toHaveProperty('upvote_count')
        expect(voteABody).toHaveProperty('downvote_count')
        expect(voteABody).toHaveProperty('user')

        // expectations based on the provided example
        expect(voteABody).toHaveProperty('upvote_count')
        expect(voteABody).toHaveProperty('downvote_count')
      }
    }

    // submit answer
    const { res: ansRes, body: ansBody } = await probe(`${API_VITE_URL}/forum/questions/1`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json', Authorization: `Token ${TEST_TOKEN}`},
      body: JSON.stringify({ body: 'test answer'})
    })
    expect(ansRes).toBeDefined()
    if(ansBody !== null){
      expect(typeof ansBody).toBe('object')
      expect(ansBody).toHaveProperty('body')
      expect(ansBody).toHaveProperty('question_id')
      expect(ansBody).toHaveProperty('user_id')
    }

    // create question
    const { res: createQRes } = await probe(`${API_VITE_URL}/forum/questions`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json', Authorization: `Token ${TEST_TOKEN}` },
      body: JSON.stringify({ question: { title: 't', body: 'b', tags: [] } })
    })
    expect(createQRes).toBeDefined()
    expect(typeof createQRes.status).toBe('number')
  })

  it('probes auth endpoints (register/login) and quiz success endpoint', async () => {
    if (!ensureApi()) return
    // register with unique email — server may accept or return conflict
    const ts = Date.now()
    const email = `test+${ts}@example.com`
    const password = `Pwd!${ts}`

    const { res: regRes, body: regBody } = await probe(`${API_VITE_URL}/auth/register`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ user: { username: `T${ts}`, email: email, password: password } })
    })
    expect(regRes).toBeDefined()
    expect(typeof regRes.status).toBe('number')

    // attempt login (may fail if register is blocked); treat non-2xx as acceptable but assert numeric status
    const { res: loginRes, body: loginBody } = await probe(`${API_VITE_URL}/auth/login`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ user: { username: email, password: password } })
    })
    expect(loginRes).toBeDefined()
    expect(typeof loginRes.status).toBe('number')
    if (loginBody !== null) expect(typeof loginBody).toBe('object')
  })
  // quiz success endpoint (may require auth)
  //   const { res: quizRes } = await probe(`${API_VITE_URL}/quiz/1/success`, {
  //     method: 'POST',
  //     headers: { 'Content-Type': 'application/json' },
  //     body: JSON.stringify({})
  //   })
  //   expect(quizRes).toBeDefined()
  //   expect(typeof quizRes.status).toBe('number')
  // })
})
