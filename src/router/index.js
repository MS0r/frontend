// src/router/index.js
import { createRouter, createWebHistory, routerKey } from 'vue-router'
import CourseLayout from '@/layouts/CourseLayout.vue'
import SubunitView from '@/views/SubunitView.vue'
import EditorLayout from '@/layouts/EditorLayout.vue'
import LoginView from '@/views/LoginView.vue'
import RegisterView from '@/views/RegisterView.vue'
import UnitExerciseView from '@/views/UnitExerciseView.vue'
import ForumLayout from '@/layouts/ForumLayout.vue'
import ForumHomeView from '@/views/ForumHomeView.vue'
import ForumQuestionView from '@/views/ForumQuestionView.vue'
import AskQuestionView from '@/views/AskQuestionView.vue'

const routes = [
  {
    path: '/',
    redirect: '/course/1/unit/1/subunit/1'
  },
  {
    path: '/course/:courseId/unit/:unitOrder/subunit/:subunitOrder',
    component: CourseLayout,
    children: [
      {
        path: '',
        component: SubunitView,
        props: true
      }
    ]
  },
  {
    path: '/course/:courseId/unit/:unitOrder/exercise',
    component: CourseLayout,
    children: [
      {
        path: '',
        component: UnitExerciseView,
        props: true
      }
    ]
  },
  {
    path: '/editor',
    component: EditorLayout
  },
  {
    path: '/login',
    component: LoginView
  },
  {
    path: '/register',
    component: RegisterView
  },
  {
    path: '/forum',
    component: ForumLayout,
    children: [
      {
        path: '',
        name: "forum-home",
        component: ForumHomeView,
        props: route => ({ searchQuery: route.query.q })
      },
      {
        path: 'question/:questionId',
        name: 'forum-question',
        component: ForumQuestionView,
        props: true
      },
      {
        path: 'ask',
        name: 'forum-ask',
        component: AskQuestionView
      }
    ]
  }
]

export default createRouter({
  history: createWebHistory(),
  routes
})
