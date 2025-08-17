<!-- src/components/Sidebar.vue -->
<template>
  <aside class="sidebar">
    <h2>{{ courseTitle }}</h2>
    <ul>
      <li v-for="unit in units" :key="unit.id">
        <details>
          <summary>{{ unit.title }}</summary>
          <ul>
            <li v-for="sub in unit.subunits" :key="sub.id">
              <router-link
                :to="`/course/1/unit/${unit.order}/subunit/${sub.order}`"
              >
                {{ sub.title }}
                <span v-if="quiz_passes_id.includes(sub.id)">✔️</span>
              </router-link>
            </li>
            <li>
              <router-link
                :to="`/course/1/unit/${unit.order}/exercise`"
              >
                Ejercicio
                <span v-if="submissions_id.includes(unit.id)">✔️</span>
              </router-link>
            </li>
          </ul>
        </details>
      </li>
    </ul>
  </aside>
</template>

<script setup>
import { useAuth } from '@/composables/useAuth';
import { onMounted, ref } from 'vue';

const { user, fetchUser } = useAuth();
let quiz_passes_id = ref([]);
let submissions_id = ref([]);

onMounted(async () =>{
  await fetchUser();
  if (user.value) {
  quiz_passes_id.value = user.value.quiz_passes.map((x) => x.quiz_id);
  submissions_id.value = user.value.submissions.map((x) => x.exercise_id);
}
  console.log(quiz_passes_id)
})


defineProps(['courseTitle','units'])
</script>

<style setup>
.sidebar {
  width: 200px;
  background: #e5e5e5; /* más similar al de la imagen */
  padding: 1rem;
  height: 100%;
  overflow-y: auto;
}

.sidebar h2 {
  margin-bottom: 1rem;
}

.sidebar ul {
  list-style: none;
  padding: 0;
}

.sidebar li {
  padding: 5px 0;
}

.sidebar details summary {
  font-weight: bold;
  cursor: pointer;
}

.router-link-active {
  background-color: #c7c7c7; /* subunidad activa */
  padding: 5px;
  border-radius: 4px;
  display: block;
}

</style>
