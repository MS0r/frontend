<!-- src/views/SubunitView.vue -->
<template>
  <main class="content">
    <div v-if="subunit">
      <div class="nav-buttons">
        <button class="nav-btn" @click="goToSubunit(-1)">Anterior</button>
        <button class="nav-btn" @click="goToSubunit(1)">Siguiente</button>
      </div>
      <h1>{{ subunit.title }}</h1>

      <div v-for="(block, idx) in subunit.blocks" :key="idx">
        <p v-if="block.type === 'text'" v-html="block.value"></p>
        <CodeBlock v-else-if="block.type === 'code'" :code="block.value" />
      </div>

      <div v-if="subunit.quiz" class="quiz card">
        <h2>{{ subunit.quiz.title }}</h2>

        <div v-for="(q, qIndex) in subunit.quiz.questions" :key="qIndex" class="quiz-question">
          <p><strong>{{ qIndex + 1 }}.</strong> {{ q.question_text }}</p>

          <div v-for="(opt, oIndex) in q.options" :key="oIndex" class="quiz-option">
            <label>
              <input type="radio"
                    :name="'q' + qIndex"
                    :value="oIndex"
                    v-model="userAnswers[qIndex]" />
              {{ opt.text }}
            </label>
          </div>
        </div>

        <button class="quiz-submit" @click="checkAnswers">Submit Answer »</button>

        <div v-if="result" class="quiz-result">
          <p>{{ result }}</p>
        </div>
      </div>

      <div class="nav-buttons">
        <button class="nav-btn" @click="goToSubunit(-1)">Anterior</button>
        <button class="nav-btn" @click="goToSubunit(1)">Siguiente</button>
      </div>
    </div>
    <div v-else>
      <h2>Seleccione una subunidad</h2>
    </div>
  </main>
</template>

<script setup>
import { useRoute, useRouter } from 'vue-router'
import CodeBlock from '@/components/CodeBlock.vue'
import { ref, watch } from 'vue'
import { useCourseStore } from '@/stores/courseStore.js'
import MarkdownIt from 'markdown-it'

const md = new MarkdownIt()
const props = defineProps(['subunit'])

const userAnswers = ref([])
const result = ref('')
const route = useRoute()
const router = useRouter()
const store = useCourseStore()

function goToSubunit(offset) {
  const courseId = route.params.courseId
  let unitOrder = parseInt(route.params.unitOrder)
  let subunitOrder = parseInt(route.params.subunitOrder)

  store.fetchCourse(courseId)
  let newSubunitOrder = subunitOrder + offset

  const actualUnit = store.units.find(u => u.order === unitOrder)
  const subunitCount = actualUnit.subunits.length  

  if (newSubunitOrder < 1) {
    // Optionally, go to previous unit's last subunit if available
    if(unitOrder - 1 > 0){
      router.push(`/course/${courseId}/unit/${unitOrder - 1}/exercise/`)
    }
    return
  }
  if (newSubunitOrder > subunitCount) {
    // Optionally, go to next unit's first subunit if available
    router.push(`/course/${courseId}/unit/${unitOrder}/exercise/`)
    return
  }
  
  router.push(`/course/${courseId}/unit/${unitOrder}/subunit/${newSubunitOrder}`)
}

async function checkAnswers() {
  if (!props.subunit.quiz) return
  let correctCount = 0

  props.subunit.quiz.questions.forEach((q, idx) => {
    const selectedIndex = userAnswers.value[idx]
    if (selectedIndex !== undefined && q.options[selectedIndex].is_correct) {
      correctCount++
    }
  })

  result.value = `You got ${correctCount} out of ${props.subunit.quiz.questions.length} correct.`
  if (correctCount === props.subunit.quiz.questions.length) {
    const token = localStorage.getItem('token')
    if(token){
      const quiz_id = props.subunit.quiz.id
      await fetch(`http://localhost:8080/api/quiz/${quiz_id}/success`, {
        method: 'POST',
        headers: {
          'Authorization': `Token ${token}`,
          'Content-Type': 'application/json'
        }
      })
    }
  }
}

watch(() => route.fullPath, () => {
  userAnswers.value = []
  result.value = ''
})

function renderMarkdown(text) {
  return md.render(text)
}
</script>

<style scoped>
.nav-buttons {
  margin-top: 20px;
  display: flex;
  justify-content: space-between;
}

.nav-btn {
  background-color: #d9d9d9ff;
  border: 1px solid #aaa;
  padding: 0.5rem 1rem;
  cursor: pointer;
}

.content {
  flex: 1;
  padding: 1rem;
  display: flex;
  flex-direction: column;
  align-items: center;
  width:100%;
}

.content > div {
  max-width: 850px;
  width: 100%;
}

.quiz {
  margin-top: 1rem;
  width: 100%;
}

.quiz-question {
  margin-bottom: 1.5rem;
}

.quiz-option {
  margin: 0.5rem 0;
  padding: 0.4rem 0.8rem;
  border-radius: 6px;
  transition: background-color 0.25s ease;
}

.quiz-option:hover {
  background-color: #f5f5f5; /* same as button bg */
}

.quiz-submit {
  margin-top: 1rem;
  background-color: #f5f5f5;
  color: black;
}

.quiz-submit:hover {
  background-color: #e0e0e0;
  border-color: #646cff;
}

.quiz-result {
  margin-top: 1rem;
  font-weight: bold;
  color: #33355e; /* matches link color */
}
</style>