import { ref, onMounted } from 'vue';

const user = ref(null);

export function useAuth() {
  const fetchUser = async () => {
    const token = localStorage.getItem('token');
    if (!token) return;

    try {
      const res = await fetch('http://localhost:8080/api/user', {
        headers: {
          'Authorization': `Token ${token}`,
        },
      });

      if (res.ok) {
        user.value = await res.json();
      } else {
        user.value = null;
      }
    } catch (err) {
      console.error(err);
      user.value = null;
    }
  };

  onMounted(fetchUser);

  return {
    user,
    fetchUser,
  };
}
