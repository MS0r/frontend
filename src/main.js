import { createApp } from 'vue'
import { createPinia, getActivePinia } from 'pinia'
import App from './App.vue'
import router from './router'
import { useCourseStore } from '@/stores/courseStore'

// ⬇️ Import global styles
import '@/assets/global.css'

const app = createApp(App)
const pinia = createPinia()
app.use(pinia)
app.use(router)

const store = useCourseStore(pinia)
store.loadFromCache(1)

app.mount('#app')