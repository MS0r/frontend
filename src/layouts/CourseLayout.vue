<!-- src/layouts/CourseLayout.vue -->
<template>
  <div class="layout">
    <Header :show-editor-btn="true" :show-forum-btn="true"/>
    <div class="main">
      <Sidebar :key="remountSidebar" :courseTitle="courseTitle" :units="store.units" />
      <RouterView v-if="selectedSubunit" :subunit="selectedSubunit" @quizSuccess="resetSidebar"/>
      <RouterView v-else-if="selectedExercise" :exercise="selectedExercise" @exerciseSuccess="resetSidebar"/>
      <Rightbar/>
    </div>
  </div>
</template>

<script setup>
import { ref, onMounted, watch } from 'vue'
import { useRoute } from 'vue-router'
import Sidebar from '@/components/Sidebar.vue'
import Header from '@/components/Header.vue'
import Rightbar from '@/components/Rightbar.vue'
import { useCourseStore } from '@/stores/courseStore.js'

const route = useRoute()
const courseTitle = ref(null)
const selectedSubunit = ref(null)
const selectedExercise = ref(null)
const store = useCourseStore()
const API_VITE_URL = import.meta.env.VITE_API_URL
const remountSidebar = ref(0)

function resetSidebar() {
  remountSidebar.value++
  console.log('reset sidebar' + remountSidebar.value)
}

const fetchCourseTitle = async () => {
  const res = await fetch(`${API_VITE_URL}/course/1`)
  const course = await res.json()
  courseTitle.value = course.title
}

const setSelectedSubunit = () => {
  selectedExercise.value = null
  selectedSubunit.value = null

  const unitOrder = parseInt(route.params.unitOrder)

  if (route.params.subunitOrder){
    const subunitOrder = parseInt(route.params.subunitOrder)
    const unit = store.units.find(u => u.order === unitOrder)
    selectedSubunit.value = unit?.subunits.find(s => s.order === subunitOrder) || null
  } else {
    const unit = store.units.find(u => u.order === unitOrder)
    selectedExercise.value = unit?.exercise || null
  }
}

onMounted(async () =>{
  await fetchCourseTitle()
  await store.fetchCourse(1)
  setSelectedSubunit()
})
watch(() => route.fullPath, setSelectedSubunit)
</script>

<style scoped>
.layout {
  display: flex;
  flex-direction: column;
  height: 100vh;
}

.main {
  display: flex;
  flex: 1;
}

</style>