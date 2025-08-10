<template>
  <div class="auth-container">
    <h2>Iniciar Sesión</h2>
    <form @submit.prevent="login">
      <input type="email" placeholder="Correo electrónico" v-model="email" required />
      <input type="password" placeholder="Contraseña" v-model="password" required />
      <button type="submit">Ingresar</button>
      <button class="back" type="back" @click="$router.push('/')">Volver</button> 
    </form>
    <p>¿No tienes una cuenta? <router-link to="/register">Regístrate</router-link></p>
  </div>
</template>

<script setup>
import { ref } from 'vue'
import { useRouter } from 'vue-router';

const email = ref('')
const password = ref('')
const router = useRouter();

const login = async () => {
  // Replace with real login logic
  const res = await fetch("http://localhost:8080/api/auth/login", {
    method: "POST",
    headers: {
      "Content-Type": "application/json"
    },
    body: JSON.stringify({user : { username: email.value, password: password.value }})
  });

  if (res.ok) {
    const user = await res.json();
    localStorage.setItem('token', user.token);
    router.push('/');
  } else {
    alert('Login failed');
  }
}

</script>

<style scoped>
.auth-container {
  max-width: 400px;
  margin: 100px auto;
  padding: 2rem;
  background: white;
  border: 1px solid #ccc;
  border-radius: 8px;
  text-align: center;
}

input {
  width: 90%;
  padding: 0.75rem;
  margin-bottom: 1rem;
  border: 1px solid #aaa;
  border-radius: 4px;
  font-size: 1rem;
}

button {
  padding: 0.75rem 1.5rem;
  background: #f0f0f0;
  border: 1px solid #ccc;
  color: black;
  cursor: pointer;
  font-size: 1rem;
}

button.back {
    margin-left: 15px;
}

button:hover {
  background: #e0e0e0;
}

a {
  color: #33355e;
}
</style>
