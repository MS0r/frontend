const API_VITE_URL = import.meta.env.VITE_API_URL

export async function compileErlang(code) {
  const res = await fetch(`${API_VITE_URL}/erlang/compile`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({"erlang_payload" : { "code" : code }}),
  });
  if (!res.ok) throw new Error("Error al compilar el código");
  return await res.json();
}

export async function testErlang(code,exercise_id) {
  const token = localStorage.getItem('token');
  const res = await fetch(`${API_VITE_URL}/exercise/${exercise_id}/submit`, {
    method: "POST",
    headers: { 
      "Content-Type": "application/json",
      "Authorization" : `Token ${token}`
    },
    body: JSON.stringify({"submission" : { "code_snippet" : code }}),
  });
  if (!res.ok) throw new Error("Error al probar el código");
  return await res.json();
}