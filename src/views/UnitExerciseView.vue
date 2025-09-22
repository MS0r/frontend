<!-- src/views/SubunitView.vue -->
<template>
  <div class="content">
    <div v-if="exercise">
      <h1>{{ exercise.title }}</h1>
        <p v-html="exercise.description"></p>
        <div class="editor-exercise">
            <div>
              <div class="monaco-container" ref="editorContainer"></div>
              <div class="nav-buttons">
                <button 
                  class="nav-btn" 
                  @click="run" 
                  :disabled="loading"
                >
                  {{ loading ? 'Ejecutando...' : 'Ejecutar Test' }}
                </button>
                <button class="nav-btn" @click="copyCode">Copiar</button>
              </div>
            </div>
            <div class="editor-box output">
                <pre>{{ loading ? 'Ejecutando Tests...' : output }}</pre>
            </div>
        </div>

    </div>
    <div v-else>
      <h2>Seleccione una subunidad</h2>
    </div>
  </div>
</template>


<script setup>
import * as monaco from 'monaco-editor';
import { onMounted, ref } from 'vue';
import { testErlang } from '@/composables/compileErlang';

const editorContainer = ref(null);
let editorInstance = null
const output = ref('');
const loading = ref(false);   // 🔹 Added loading state
const props = defineProps(['exercise']);
const emit = defineEmits(['exerciseSuccess'])

async function copyCode() {
  try{
    await navigator.clipboard.writeText(editorInstance.getValue());
    alert('Código copiado al portapapeles');
  } catch (err) {
    alert('Error al copiar el código: ' + err);
  }
}


onMounted(() => {
  monaco.languages.register({ id: 'erlang' });

  monaco.languages.setMonarchTokensProvider('erlang', {
    tokenizer: {
      root: [
        [/%.*$/, 'comment'],
        [/\b(module|export|import|fun|case|of|when|end|receive|after|try|catch|throw|if|compile|record|spec)\b/, 'keyword'],
        [/[A-Z][A-Za-z0-9_]*/, 'variable'],// variables
        [/"([^"\\]|\\.)*"/, 'string'],
        [/[0-9]+/, 'number'],
        [/[(),.;]/, 'delimiter'],
        [/->/, 'operator'],
        [/(?<=-export\(\[)[a-z][A-Za-z0-9_]*(?=\/\d)/, 'function.name'],
        [/\b[a-z][A-Za-z0-9_]*(?=\()/, 'function.name'],
      ],
    },
  });

  editorInstance = monaco.editor.create(editorContainer.value, {
    value: `${props.exercise.exercise_schema}`,
    language: 'erlang',
    theme: 'vs-dark',
    minimap: { enabled: false },
    lineNumbersMinChars: 2,
    lineNumbers: "on",
    fontSize: 14,  
    wordWrap: 'on',
    scrollbar: {
      vertical: 'auto',
      horizontal: 'auto',
      alwaysConsumeMouseWheel: false,
    }
  });
});

async function run(){
  try{
    loading.value = true;         // 🔹 Start loading
    output.value = '';            // clear previous output
    const code = editorInstance.getValue();
    const result = await testErlang(code, props.exercise.id);
    output.value = result.result || 'Sin salida';
    if (result.test_results.failures === 0){
      emit("exerciseSuccess")
    }
  } catch (error) {
    output.value = 'Error: ' + error.message;
  } finally {
    loading.value = false;        // 🔹 Stop loading
  }
}
</script>

<style scoped>
.nav-buttons {
  margin-top: 20px;
  display: flex;
  gap: 1rem;
}

.nav-btn:disabled {
  opacity: 0.6;
  cursor: not-allowed;
}

.editor-exercise {
  display: flex;
  background: #ffffffff;
  gap: 1rem;
}

.monaco-container {
  height: 500px;
  width: 450px; 
  max-width: 100%;
  flex: 1;
}

.editor-box {
  background: #252525;
  color: white;
  flex: 1;
  padding: 1rem;
  overflow: auto;
  font-family: monospace;
  font-size: 14px;
  white-space: pre-wrap;
  word-wrap: break-word;
}

.output {
  width: 250px;
  display: flex;
  min-width: 350px;
  max-width: 100%;
  flex: none;
}

.editor-box pre{
  white-space: pre-wrap;
  word-wrap: break-word;
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
  max-width: 900px;
  width: 100%;
}
</style>
