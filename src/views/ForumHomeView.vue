<template>
  <div>
    <h2 class="section-title">Nuevas Preguntas</h2>
    <div class="question-list">
      <div
        v-for="q in questions"
        :key="q.id"
        class="question-item"
      >
        <!-- Stats column -->
        <div class="question-stats">
          <div class="stat">
            <span class="stat-number">{{ q.upvote_count - q.downvote_count }}</span>
            <span class="stat-label">votos</span>
          </div>
          <div class="stat">
            <span class="stat-number">{{ q.answer_count }}</span>
            <span class="stat-label">respuestas</span>
          </div>
        </div>

        <!-- Info column -->
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
const props = defineProps(["questions", "searchQuery"]);
</script>

<style scoped>
.section-title {
  font-size: 2rem;
  font-weight: bold;
  margin-bottom: 20px;
}

/* Question container */
.question-item {
  display: flex;
  gap: 20px;
  border-top: 1px solid #ddd;
  padding: 20px 0;
}

/* Stats (votes, answers) */
.question-stats {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  min-width: 90px;
  font-size: 1rem;
  color: #444;
  text-align: center;
}

.stat {
  margin-bottom: 10px;
}

.stat-number {
  font-size: 1.5rem;
  font-weight: bold;
  display: block;
}

.stat-label {
  font-size: 0.9rem;
  color: #777;
}

/* Question info */
.question-info {
  flex: 1;
}

.question-title {
  font-size: 1.4rem;
  font-weight: bold;
  color: #1a73e8;
  text-decoration: none;
}

.question-title:hover {
  text-decoration: underline;
}

.question-description {
  font-size: 1.1rem;
  color: #555;
  margin: 5px 0 10px;
}

.question-tags {
  margin-top: 8px;
}

.tag {
  background: #eef;
  padding: 6px 10px;
  margin-right: 6px;
  border-radius: 6px;
  font-size: 0.9rem;
}
</style>
