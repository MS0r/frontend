<!-- src/components/CodeBlock.vue -->
<template>
  <div class="code-box">
    <pre><code>{{ code }}</code></pre>
    <button @click="runCode" id="execute">Ejecutar</button>
    <button @click="copyCode" id="copy">Copiar</button>
    <pre v-show="output" class="output">{{ output }}</pre>
  </div>
</template>

<script setup>
import { useRoute } from 'vue-router';
import { compileErlang } from '@/composables/compileErlang';
import { ref, watch } from 'vue';

const output = ref(null);
const props = defineProps(['code'])
const route = useRoute();

async function copyCode() {
  try{
    await navigator.clipboard.writeText(editorInstance.getValue());
    alert('Código copiado al portapapeles');
  } catch (err) {
    alert('Error al copiar el código: ' + err);
  }
}

async function runCode(){
  try{
    const result = await compileErlang(props.code);
    if(result.status === "ok"){
      output.value = result.result || 'Sin salida';
    } else if (result.status === "error") {
      output.value = `Error: ${result.reason}`;
    }
  } catch (error) {
    console.error(error);
  }
}

watch(() => route.fullPath, () => {
  output.value = ''
})
</script>

<style>
.code-box {
  background-color: #252525;
  padding: 1rem;
  border-radius: 5px;
  margin-top: 1rem;
  position: relative;
}

.code-box pre {
  white-space: pre-wrap;
  font-size: 16px;
}

.output {
  background-color: #383838ff;
  padding: 15px;
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