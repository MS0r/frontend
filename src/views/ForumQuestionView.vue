<template>
  <div v-if="question">
    <button @click="goBack" class="back-btn">⬅ Volver</button>

    <!-- Pregunta -->
    <div class="question-container">
      <div class="vote-section">
        <button 
          @click="voteQuestion('upvote')" 
          id="upvote-q"
          :class="{ active: userQuestionVote === 'upvote' }"
        >⬆</button>
        <span>{{ question.upvote_count - question.downvote_count }}</span>
        <button 
          @click="voteQuestion('downvote')" 
          id="downvote-q"
          :class="{ active: userQuestionVote === 'downvote' }"
        >⬇</button>
      </div>
      <div class="question-content">
        <h1 class="question-title">{{ question.title }}</h1>
        <p class="question-user">Preguntado por <strong>{{ question.user }}</strong></p>
        <p class="question-description">{{ question.body }}</p>
        <div class="question-tags">
          <span v-for="(tag, i) in question.tags" :key="i" class="tag">
            {{ tag }}
          </span>
        </div>
      </div>
    </div>

    <hr />
    <h2>Respuestas</h2>

    <!-- Respuestas -->
    <div v-if="question.answers.length">
      <div v-for="(c, i) in question.answers" :key="c.id || i" class="answer">
        <div class="vote-section">
          <button 
            @click="voteAnswer(c.id, i, 'upvote')" 
            :id="`upvote-a-${i}`"
            :class="{ active: userAnswerVotes[c.id] === 'upvote' }"
          >⬆</button>
          <span>{{ (c.upvote_count || 0) - (c.downvote_count || 0) }}</span>
          <button 
            @click="voteAnswer(c.id, i, 'downvote')" 
            :id="`downvote-a-${i}`"
            :class="{ active: userAnswerVotes[c.id] === 'downvote' }"
          >⬇</button>
        </div>
        <div class="answer-content">
          <p class="answer-user"><strong>{{ c.user }}</strong></p>
          <p class="answer-text">{{ c.body }}</p>
        </div>
      </div>
    </div>
    <p v-else>Aún no hay respuestas.</p>

    <hr />
    <h2>Tu respuesta</h2>
    <form @submit.prevent="submitAnswer" class="answer-form">
      <textarea
        v-model="newAnswer"
        placeholder="Escribe tu respuesta aquí..."
        required
      ></textarea>
      <button type="submit">Responder</button>
    </form>
  </div>
</template>

<script setup>
import { ref, onMounted } from "vue";
import { useRoute, useRouter } from "vue-router";

const route = useRoute();
const router = useRouter();
const question = ref(null);
const userQuestionVote = ref(null);
const userAnswerVotes = ref({});
const newAnswer = ref("");
const API_VITE_URL = import.meta.env.VITE_API_URL;
const token = localStorage.getItem("token");

const goBack = () => {
  if (window.history.length > 1) {
    router.back();
  } else {
    router.push("/forum");
  }
};

const fetchQuestion = async () => {
  const res = await fetch(
    `${API_VITE_URL}/forum/questions/view/${route.params.questionId}`
  );
  question.value = await res.json();

  if (token) {
    const votesRes = await fetch(`${API_VITE_URL}/forum/vote/${route.params.questionId}`, {
      headers: { Authorization: `Token ${token}` },
    });
    let votes = await votesRes.json();

    // 🔹 Extract user's existing votes
    const qVote = votes.find(v => v.question_id === question.value.id);
    if (qVote) userQuestionVote.value = qVote.vote;

    votes
      .filter(v => v.answer_id !== null)
      .forEach(v => {
        userAnswerVotes.value[v.answer_id] = v.vote;
      });
  }
};

// 🔹 Votar pregunta
const voteQuestion = async (voteType) => {
  if (token) {
    if (userQuestionVote.value === voteType) {
      userQuestionVote.value = "";
    } else {
      userQuestionVote.value = voteType;
    }

    const res = await fetch(`${API_VITE_URL}/forum/vote/${route.params.questionId}`, {
      method: "POST",
      headers: { "Content-Type": "application/json", Authorization: `Token ${token}` },
      body: JSON.stringify({ vote: voteType }),
    });

    if (res.ok) {
      const result = await res.json();
      question.value.upvote_count = result.upvote_count;
      question.value.downvote_count = result.downvote_count;
    } else {
      alert("Error al votar la pregunta");
    }
  }
};

// 🔹 Votar respuesta
const voteAnswer = async (answerId, index, voteType) => {
  if (token) {

    if (userAnswerVotes.value[answerId] === voteType) {
      userAnswerVotes.value[answerId] = "";
    } else {
      userAnswerVotes.value[answerId] = voteType;
    }
    const res = await fetch(`${API_VITE_URL}/forum/vote/answer/${answerId}`, {
      method: "POST",
      headers: { "Content-Type": "application/json", Authorization: `Token ${token}` },
      body: JSON.stringify({ vote: voteType }),
    });

    if (res.ok) {
      const result = await res.json();
      question.value.answers[index].upvote_count = result.upvote_count;
      question.value.answers[index].downvote_count = result.downvote_count;
      userAnswerVotes.value[answerId] = voteType;
    } else {
      alert("Error al votar la respuesta");
    }
  }
};

const submitAnswer = async () => {
  if (!newAnswer.value.trim()) return;
  if (token) {
    const res = await fetch(`${API_VITE_URL}/forum/questions/${route.params.questionId}`, {
      method: "POST",
      headers: { "Content-Type": "application/json", Authorization: `Token ${token}` },
      body: JSON.stringify({
        body: newAnswer.value,
      }),
    });

    if (res.ok) {
      const answer = await res.json();
      console.log(answer)
      question.value.answers.push(answer);
      newAnswer.value = "";
    } else {
      alert("Error al enviar la respuesta");
    }
  }
};

onMounted(fetchQuestion);
</script>

<style scoped>
.back-btn {
  background: white;
  border: 1px solid #ccc;
  padding: 8px 16px;
  border-radius: 6px;
  cursor: pointer;
  margin-bottom: 20px;
  font-size: 1.1rem;
}

.back-btn:hover {
  background-color: #e0e0e0;
  border-color: #646cff;
}

/* ---- Pregunta y respuesta layout estilo SO ---- */
.question-container,
.answer {
  display: flex;
  gap: 20px;
  margin-bottom: 25px;
}

.question-content,
.answer-content {
  flex: 1;
}

.vote-section {
  display: flex;
  flex-direction: column;
  align-items: center;
  font-size: 1.8rem;
  user-select: none;
  min-width: 50px;
}

.vote-section button {
  background: none;
  border: none;
  cursor: pointer;
  font-size: 2.2rem;
  color: #666;
  transition: color 0.2s;
}

.vote-section button:hover {
  color: #afd9ffff;
}

.vote-section .active {
  color: #0084ffff;
}

.vote-section span {
  font-size: 1.5rem;
  font-weight: bold;
  margin: 8px 0;
}

/* ---- Texto ---- */
.question-title {
  font-size: 1.8rem;
  font-weight: bold;
}

.question-user {
  font-size: 1.2rem;
  color: #444;
  margin-bottom: 10px;
}

.question-description {
  font-size: 1rem;
  margin-bottom: 25px;
}

.answer-user {
  font-size: 1.1rem;
}

.answer-text {
  font-size: 1rem;
}

/* ---- Tags ---- */
.question-tags {
  margin: 12px 0;
}

.tag {
  background: #eef;
  padding: 6px 12px;
  margin-right: 6px;
  border-radius: 6px;
  font-size: 1rem;
}

/* ---- Contenedor de respuesta ---- */
.answer {
  padding: 12px;
  border-left: 3px solid #ddd;
}

/* ---- Formulario ---- */
.answer-form {
  display: flex;
  flex-direction: column;
  gap: 12px;
}

.answer-form textarea {
  min-height: 120px;
  padding: 12px;
  border-radius: 6px;
  border: 1px solid #ccc;
  font-size: 1.2rem;
  background-color: white;
  color:black;
}

.answer-form button {
  align-self: flex-start;
  padding: 8px 16px;
  background: #4caf50;
  color: white;
  border: none;
  border-radius: 6px;
  cursor: pointer;
  font-size: 1.1rem;
}
</style>
