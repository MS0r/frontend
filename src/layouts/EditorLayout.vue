<template>
  <div class="editor-layout">
    <Header :show-tutorial-btn="true" :show-forum-btn="true"/>

    <div class="editor-toolbar">
      <button @click="run">Ejecutar</button>
      <button>Compartir</button>
    </div>

    <div class="editor-main">
      <div class="editor-box" ref="editorContainer"></div>
      <div class="editor-box output">
        <pre>{{ output }}</pre>
      </div>
      <div class="placeholder"></div>
    </div>
  </div>
</template>

<script setup>
import * as monaco from 'monaco-editor';
import { onMounted, ref } from 'vue';
import Header from '@/components/Header.vue';
import { compileErlang } from '@/composables/compileErlang';

const editorContainer = ref(null);
let editorInstance = null
const output = ref('Hello World!\n');

onMounted(() => {
  monaco.languages.register({ id: 'erlang' });

  monaco.languages.setMonarchTokensProvider('erlang', {
    tokenizer: {
      root: [
        [/%.*$/, 'comment'],
        [/\b(module|export|import|fun|case|of|when|end|receive|after|try|catch|throw|if|compile|record|spec)\b/, 'keyword'],
        //[/[a-z][A-Za-z0-9_]*/, 'type.identifier'], // module names, atoms
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

  monaco.editor.defineTheme('erlangFunctions', {
  base: 'vs-dark',
  inherit: true,
  rules: [
    { token: 'function.name', foreground: 'FFCC00', fontStyle: 'bold' }
  ]
  });
  

  editorInstance = monaco.editor.create(editorContainer.value, {
    value: `% hello world program
-module(helloworld).
-export([start/0]).

start() ->
    io:format("Hello, world!~n").`,
    language: 'erlang',
    theme: 'vs-dark',
    automaticLayout: true,
  });

  
});

const run = async () => {
  try {
    const code = editorInstance.getValue();
    console.log("enviando")
    const result = await compileErlang(code);
    if(result.status === "ok"){
      output.value = result.result || 'Sin salida';
    } else if (result.status === "error") {
      output.value = 'Error: ' + result.reason;
    }

  } catch (e) {
    output.value = 'Error: ' + e.message;
  }
}
</script>

<style scoped>
.editor-layout {
  height: 100vh;
  display: flex;
  flex-direction: column;
}

.header {
  background: #e5e5e5;
  padding: 1rem;
  display: flex;
  justify-content: space-between;
}

.editor-toolbar {
  background: #dcdcdc;
  padding: 1rem;
  display: flex;
  gap: 1rem;
}


.editor-main {
  flex: 1;
  display: flex;
  background: #dcdcdc;
  gap: 1rem;
  padding: 1rem;
}

.editor-box {
  background: #1e1e1e;
  color: white;
  flex: 1;
  padding: 1rem;
  overflow: auto;
  font-family: monospace;
  font-size: 14px;
}

.output {
  white-space: pre-wrap;
}

.placeholder {
    width: 350px;
}
</style>
