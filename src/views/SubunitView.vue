<!-- src/views/SubunitView.vue -->
<template>
  <main class="content">
    <div v-if="subunit">
      <div class="nav-buttons">
        <button class="nav-btn">Anterior</button>
        <button class="nav-btn">Siguiente</button>
      </div>
      <h1>{{ subunit.title }}</h1>

      <div v-for="(block, idx) in subunit.blocks" :key="idx">
        <p v-if="block.type === 'text'">{{ block.value }}</p>
        <CodeBlock v-else-if="block.type === 'code'" :code="block.value" />
      </div>
      <div class="nav-buttons">
        <button class="nav-btn">Anterior</button>
        <button class="nav-btn">Siguiente</button>
      </div>
    </div>
    <div v-else>
      <h2>Seleccione una subunidad</h2>
    </div>
  </main>
</template>

<script setup>
import CodeBlock from '@/components/CodeBlock.vue'
defineProps(['subunit'])
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
  max-width: 700px;
  width: 100%;
  transform: translateX(-30px); /* slight offset to the left */
}
</style>