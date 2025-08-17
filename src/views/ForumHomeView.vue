<template>
  <div>
    <h2 class="section-title">Nuevas Preguntas</h2>
    <div class="question-list">
      <div
        v-for="q in questions"
        :key="q.id"
        class="question-item"
      >
        <div class="question-stats">
          <span>{{ q.upvote_count }} 👍</span>
          <span>{{ q.downvote_count }} 👎</span>
          <span>{{ q.answer_count }} 💬</span>
        </div>

        <div class="question-info">
          <router-link
            :to="{ name: 'forum-question', params: { questionId: q.id } }"
            class="question-title"
          >
            {{ q.title }}
          </router-link>
          <p class="question-description">{{ q.body }}</p>

          <div class="question-tags">
            <span v-for="(tag, i) in q.tags" :key="i" class="tag">
              {{ tag }}
            </span>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script setup>
const props = defineProps(['questions','searchQuery'])
</script>

<style scoped>
.section-title {
  font-size: 1.5rem;
  font-weight: bold;
  margin-bottom: 15px;
}

.question-item {
  border-top: 1px solid #ccc;
  padding: 10px 0;
}

.question-stats {
  font-size: 0.85rem;
  color: gray;
  display: flex;
  gap: 15px;
  margin-bottom: 5px;
}

.question-title {
  font-size: 1.2rem;
  font-weight: bold;
  color: #1a73e8;
  text-decoration: none;
}

.question-description {
  margin: 0;
  color: #555;
}

.question-tags {
  margin-top: 5px;
}

.tag {
  background: #eef;
  padding: 4px 8px;
  margin-right: 5px;
  border-radius: 5px;
  font-size: 0.75rem;
}
</style>
