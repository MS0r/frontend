<!-- src/components/Header.vue -->
<template>
  <header class="header">
    <div>
      <button @click="$router.push('/forum')">Foro</button>
      <button 
        v-if="showEditorBtn"
        @click="$router.push('/editor')"
      >
      Editor
      </button>
      <button
        v-if="showTutorialBtn"
        @click="$router.push('/course/1/unit/1/subunit/1')"
      >
      Tutorial
      </button>
    </div>
    <div v-if="!user">
      <button @click="$router.push('/register')">Registrarse</button>
      <button @click="$router.push('/login')">Iniciar Sesión</button>
    </div>

    <div v-else class="user-dropdown" ref="dropdownRef">
      <button @click="toggleDropdown">
        Perfil
        <span :class="{'arrow-up': showDropdown, 'arrow-down': !showDropdown}"></span>
      </button>

      <div v-if="showDropdown" class="dropdown-content styled-dropdown">
        <h3>{{user.username}}</h3>
        <div class="progress-wrapper grey-back">
          <span>Tutorial de Erlang</span>
          <div class="progress-bar">
            <div class="progress-fill" :style="{  width : (progress.progress || 0) + '%'}"></div>
          </div>
          <span class="small">{{progress.progress}}% Completado</span>
        </div>
        <div class="stats">
          <div class="grey-back">
            <p><strong>Tiempo invertido:</strong></p>
            <p>00:00:00</p>
          </div>
          <div class="grey-back">
            <p><strong>Cuestionarios Completados:</strong></p>
            <p>{{progress.completed_quizzes}} / {{progress.total_quizzes}}</p>
          </div>
          <div class="grey-back">
            <p><strong>Ejercicios Completados:</strong></p>
            <p>{{progress.completed_exercises}} / {{progress.total_exercises}}</p>
          </div>
        </div>
        <button class="logout-btn" @click="logout">Cerrar Sesión</button>
      </div>
    </div>
  </header>
</template>

<script setup>
import { ref, onMounted } from 'vue'
import { useAuth } from '@/composables/useAuth';
import { useRouter } from 'vue-router';

const props = defineProps({
  showEditorBtn: Boolean,
  showTutorialBtn: Boolean,
});

const router = useRouter();
const { user } = useAuth();
const progress = ref({})

const logout = () => {
  localStorage.removeItem('token');
  user.value = null;
  router.push('/login');
};

const showDropdown = ref(false); // <-- dropdown visibility
const toggleDropdown = () => {
  showDropdown.value = !showDropdown.value;
};

const getProgress = async () => {
  const token = localStorage.getItem('token');
  const res = await fetch("http://localhost:8080/api/user/progress/1", {
    headers: {
      'Authorization': `Token ${token}`
    }
  });

  if(res.ok){
    progress.value = await res.json()
  }
}

onMounted(getProgress)
</script>

<style>
.header {
  display: flex;
  justify-content: space-between;
  padding: 1rem;
  background: #d9d9d9;
}

.header button {
  margin-left: 10px;
  padding: 0.5rem 1rem;
  background-color: white;
  border: 1px solid #ccc;
  cursor: pointer;
}

.user-dropdown {
  position: relative;
}

.user-dropdown button {
  background: none;
  border: none;
  cursor: pointer;
  font-weight: bold;
  display: flex;
  align-items: center;
}

.arrow-down::after {
  content: "▼";
  font-size: 0.7rem;
  margin-left: 5px;
}
.arrow-up::after {
  content: "▲";
  font-size: 0.7rem;
  margin-left: 5px;
}

.dropdown-content.styled-dropdown {
  position: absolute;
  top: 100%;
  right: 0;
  background-color: #fff;
  width: 280px;
  padding: 1rem;
  box-shadow: 0 8px 16px rgba(0,0,0,0.2);
  z-index: 999;
  border-radius: 4px;
}

.dropdown-content h3 {
  margin-bottom: 1rem;
}

.progress-wrapper {
  margin-bottom: 1rem;
}

.progress-bar {
  height: 6px;
  background: #ccc;
  border-radius: 3px;
  overflow: hidden;
  margin: 4px 0;
}

.progress-fill {
  height: 6px;
  background-color: #4caf50;
}

.small {
  font-size: 0.75rem;
  color: #666;
}

.stats p {
  margin: 0.25rem 0;
}

.logout-btn {
  margin-top: 1rem;
  color: #d33;
  background: none;
  border: none;
  cursor: pointer;
  font-weight: bold;
}

.grey-back {
  background-color: #f3f2f2ff;
  margin-bottom: 1rem;
  padding: 10px;
}

</style>
