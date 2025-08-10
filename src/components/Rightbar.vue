<!-- src/components/Sidebar.vue -->
<template>
  <aside class="rightbar">
  </aside>
</template>

<script setup>
defineProps(['courseTitle','units'])
</script>

<style scoped>
.rightbar {
  width: 150px;
  background: #e5e5e5; /* más similar al de la imagen */
  padding: 1rem;
  height: 100%;
  overflow-y: auto;
}

</style>
