<template>
  <div class="layout">
    <Header 
      :show-editor-btn="true" 
      :show-tutorial-btn="true" 
      v-model="searchQuery" 
      @search="performSearch"
    />

    <div class="forum-layout">
      <aside class="left-sidebar"></aside>

      <main class="forum-main">
        <RouterView
          :questions="filteredQuestions"
          :searchQuery="searchQuery"
        />
      </main>

      <aside class="right-sidebar">
        <router-link :to="{ name: 'forum-ask' }" class="create-btn">
          Crear Pregunta
        </router-link>
      </aside>
    </div>
  </div>
</template>

<script setup>
import { ref, onMounted } from "vue";
import Header from "@/components/Header.vue";

const searchQuery = ref("");
const filteredQuestions = ref([]);
const API_VITE_URL = import.meta.env.VITE_API_URL

const fetchQuestions = async () => {
  const res = await fetch(`${API_VITE_URL}/forum/questions`);
  const results = await res.json();
  filteredQuestions.value = results; // default list
}

const performSearch = async () => {
  if(searchQuery.value){
    const res = await fetch(`${API_VITE_URL}/forum/questions?s=${searchQuery.value}`);
    const results = await res.json();
    filteredQuestions.value = results;
  }
  
// if (!searchQuery.value) {
//   filteredQuesxtions.value = questions.value;
// } else {
//   const q = searchQuery.value.toLowerCase();
//   filteredQuestions.value = questions.value.filter(item =>
//     item.title.toLowerCase().includes(q) ||
//     item.body.toLowerCase().includes(q) ||
//     (item.tags && item.tags.some(tag => tag.toLowerCase().includes(q)))
//   );
// }
};

onMounted(fetchQuestions);
</script>

<style scoped>
.layout {
  display: flex;
  flex-direction: column;
  height: 100vh;
}

.forum-layout {
  display: grid;
  grid-template-columns: 200px 1fr 200px;
  min-height: calc(100vh - 60px); /* Adjust for Header */
}

.left-sidebar {
  background-color: #f3f3f3;
}

.forum-main {
  background-color: white;
  padding: 20px;
}

.right-sidebar {
  background-color: #f3f3f3;
  justify-content: center;
}

/* .create-btn {
  background-color: white;
  border: 1px solid #ccc;
  display: flex;
  border-radius: 8px;
  cursor: pointer;
  justify-content: center;
  height: 5%;
  margin-top: 15px;
  margin-left: 15px;

} */

.create-btn {
  display: block;
  text-align: center;
  padding: 12px;
  margin: 20px auto;
  background-color: white;
  color: black;
  font-weight: 500;
  font-size: 1em;
  border: 1px solid transparent;
  transition: background-color 0.25s, border-color 0.25s;
  border-radius: 8px;
  cursor: pointer;
  width: 80%;
}

.create-btn:hover {
  background-color: #e0e0e0;
  border-color: #646cff;
}

</style>
