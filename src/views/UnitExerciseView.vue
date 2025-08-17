<!-- src/views/SubunitView.vue -->
<template>
  <div class="content">
    <div v-if="exercise">
      <h1>{{ exercise.title }}</h1>
        <p v-html="exercise.description"></p>
        <div class="editor-exercise">
            <div class="monaco-container" ref="editorContainer"></div>
            <div class="editor-box output">
                <pre>{{ output }}</pre>
            </div>
        </div>
        <div class="nav-buttons">
          <button class="nav-btn" @click="run">Ejecutar Test</button>
          <button class="nav-btn" @click="copy">Copiar</button>
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
import MarkdownIt from 'markdown-it';

const md = MarkdownIt();
const editorContainer = ref(null);
let editorInstance = null
const output = ref('');
const props = defineProps(['exercise']);

onMounted(() => {
  monaco.languages.register({ id: 'erlang' });

  monaco.languages.setMonarchTokensProvider('erlang', {
    tokenizer: {
      root: [
        [/%.*$/, 'comment'],
        [/\b(module|export|import|fun|case|of|when|end|receive|after|try|catch|throw|if)\b/, 'keyword'],
        [/[A-Z][A-Za-z0-9_]*/, 'type.identifier'], // module names, atoms
        [/[a-z][A-Za-z0-9_]*/, 'identifier'], // variables
        [/".*?"/, 'string'],
        [/[0-9]+/, 'number'],
        [/[(),.;]/, 'delimiter'],
        [/->/, 'operator'],
      ],
    },
  });

  editorInstance = monaco.editor.create(editorContainer.value, {
    value: `${props.exercise.exercise_schema}`,
    language: 'erlang',
    theme: 'vs-dark',
    minimap: { enabled: false }, // Hide minimap
    lineNumbersMinChars: 2,      // Make line numbers column smaller
    lineNumbers: "on",           // Show line numbers
    fontSize: 14,  
  });
});

async function run(){
  try{
    const code = editorInstance.getValue();
    const result = await testErlang(code, props.exercise.id);
    output.value = result.result || 'Sin salida';
  } catch (error) {
    output.value = 'Error: ' + error.message;
  }
}

function renderMarkdown(text) {
  return md.render(text)
} 
</script>

<style scoped>
.nav-buttons {
  margin-top: 20px;
  display: flex;
  gap: 1rem;
}

.editor-exercise {
  display: flex;
  background: #ffffffff;
  gap: 1rem;
}

.monaco-container {
  height: 300px;
  width: 450px; 
  min-width: 400px;
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