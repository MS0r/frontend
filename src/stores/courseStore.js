import { defineStore } from 'pinia'

const CACHE_TTL = 1000 * 60 * 5

export const useCourseStore = defineStore('course', {
    state: () => ({
        units: [],
    }),
    actions: {
        loadFromCache(courseId) {
            const cached = localStorage.getItem(`course:${courseId}`)
            if (cached) {
                const { timestamp, data } = JSON.parse(cached)
                if( Date.now() - timestamp < CACHE_TTL) {
                    this.units = data
                    return true
                } else {
                    localStorage.removeItem(`course:${courseId}`)
                }
            }
            return false
        },
        async fetchCourse(courseId) {
            const loaded = this.loadFromCache(courseId)
            if (!loaded) {
                const res = await fetch(`http://localhost:8080/api/course/${courseId}/units`)
                this.units = await res.json()

                localStorage.setItem(`course:${courseId}`, JSON.stringify({
                    timestamp: Date.now(),
                    data : this.units
                }))
            }
        }
    }
})