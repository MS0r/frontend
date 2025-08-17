<template>
  <div v-if="question">
    <button @click="goBack" class="back-btn">⬅ Volver</button>
    <h1 class="question-title">{{ question.title }}</h1>
    <p class="question-user">Preguntado por <strong>{{ question.user }}</strong></p>

    <div class="question-stats">
      <span>{{ question.upvote_count }} 👍</span>
      <span>{{ question.downvote_count }} 👎</span>
      <span>{{ question.answers.length }} 💬</span>
      <span>{{ question.views || 0 }} 👁</span>
    </div>

    <p class="question-description">{{ question.body }}</p>

    <div class="question-tags">
      <span v-for="(tag, i) in question.tags" :key="i" class="tag">
        {{ tag }}
      </span>
    </div>

    <hr />
    <h2>Respuestas</h2>
    <div v-if="question.answers.length">
      <div v-for="(c, i) in question.answers" :key="c.id || i" class="answer">
        <p><strong>{{ c.user }}</strong></p>
        <p>{{ c.body }}</p>
      </div>
    </div>
    <p v-else>Aún no hay respuestas.</p>
  </div>
</template>

<script setup>
import { ref, onMounted } from "vue";
import { useRoute, useRouter } from "vue-router";

const route = useRoute();
const router = useRouter();
const question = ref(null);

const goBack = () => {
  if (window.history.length > 1) {
    router.back();
  } else {
    router.push("/forum");
  }
};

const fetchQuestion = async () => {
  const res = await fetch(
    `http://localhost:8080/api/forum/questions/view/${route.params.questionId}`
  );
  question.value = await res.json();
};

onMounted(fetchQuestion);
</script>

<style scoped>
.back-btn {
  background: white;
  border: 1px solid #ccc;
  padding: 6px 12px;
  border-radius: 6px;
  cursor: pointer;
  margin-bottom: 15px;
}

.question-title {
  font-size: 1.8rem;
  font-weight: bold;
}

.question-user {
  font-size: 0.95rem;
  color: #444;
  margin-bottom: 5px;
}

.question-stats {
  color: gray;
  display: flex;
  gap: 15px;
  font-size: 0.9rem;
  margin: 10px 0;
}

.question-description {
  font-size: 1rem;
  margin-bottom: 20px;
}

.question-tags {
  margin: 10px 0;
}

.tag {
  background: #eef;
  padding: 5px 10px;
  margin-right: 5px;
  border-radius: 5px;
  font-size: 0.8rem;
}

.answer {
  margin-bottom: 15px;
  padding: 10px;
  border-left: 3px solid #ddd;
}
</style>
