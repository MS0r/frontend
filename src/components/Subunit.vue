<!-- src/components/Subunit.vue -->
<template>
  <div>
    <h2>{{ subunit.title }}</h2>

    <div v-for="(block, i) in subunit.blocks" :key="i">
      <p v-if="block.type === 'text'">{{ block.value }}</p>
      <CodeBlock v-else-if="block.type === 'code'" :code="block.value" />
    </div>
  </div>
</template>

<script setup>
import CodeBlock from './CodeBlock.vue'
defineProps({ subunit: Object })
</script>
