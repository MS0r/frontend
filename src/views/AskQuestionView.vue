<template>
  <div class="ask-question">
    <button @click="goBack" class="back-btn">⬅ Volver</button>
    <h2>Crear Pregunta</h2>

    <form @submit.prevent="submitQuestion">
      <label>Título</label>
      <input v-model="title" required />

      <label>Descripción</label>
      <textarea v-model="body" required></textarea>

      <label>Tags (separados por coma)</label>
      <input v-model="tags" />

      <button type="submit">Publicar</button>
    </form>
  </div>
</template>

<script setup>
import { ref } from "vue";
import { useRouter } from "vue-router";

const title = ref("");
const body = ref("");
const tags = ref("");
const router = useRouter();

const goBack = () => {
  if (window.history.length > 1) {
    router.back();
  } else {
    router.push("/forum");
  }
};

const submitQuestion = async () => {
    const token = localStorage.getItem('token');
  await fetch("http://localhost:8080/api/forum/questions", {
    method: "POST",
    headers: { "Content-Type": "application/json", "Authorization": `Token ${token}` },
    body: JSON.stringify({question : {
      title: title.value,
      body: body.value,
      tags: tags.value.split(",").map(t => t.trim())
    }})
  });

  router.push("/forum"); // back to list after creating
};
</script>

<style scoped>
.ask-question {
  max-width: 600px;
  margin: 0 auto;
}

.back-btn {
  background: white;
  border: 1px solid #ccc;
  padding: 6px 12px;
  border-radius: 6px;
  cursor: pointer;
  margin-bottom: 15px;
}

form {
  display: flex;
  flex-direction: column;
  gap: 15px;
}

input, textarea {
  padding: 10px;
  border: 1px solid #ccc;
  border-radius: 6px;
  color: black;
  background-color: white;
}
</style>
