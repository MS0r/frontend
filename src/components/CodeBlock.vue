<!-- src/components/CodeBlock.vue -->
<template>
  <div class="code-box">
    <pre><code>{{ code }}</code></pre>
    <button @click="runCode">Ejecutar</button>
    <button @click="copy">Copiar</button>
    <div v-if="output">
      <pre>{{ output }}</pre>
    </div>
  </div>
</template>

<script setup>
import { compileErlang } from '@/composables/compileErlang';
import { ref } from 'vue';

const output = ref('');
const props = defineProps(['code'])

async function runCode(){
  try{
    const result = await compileErlang(props.code);
    if(result.status === "ok"){
      output.value = result.result || 'Sin salida';
    } else if (result.status === "error") {
      output.value = 'Error: ' + result.reason;
    }
  } catch (error) {
    console.error(error);
  }
}

</script>

<style>
.code-box {
  background-color: #1e1e1e;
  color: #d4d4d4;
  padding: 1rem;
  border-radius: 5px;
  margin-top: 1rem;
  position: relative;
}

.code-box pre {
  font-family: monospace;
  white-space: pre-wrap;
}

.code-box button {
  margin-left: 10px;
  background-color: #444;
  color: white;
  border: none;
  padding: 0.3rem 0.6rem;
  cursor: pointer;
}

.code-box > div:last-child {
  background-color: black;
  color: white;
  padding: 0.5rem;
  margin-top: 10px;
  font-family: monospace;
}

</style>