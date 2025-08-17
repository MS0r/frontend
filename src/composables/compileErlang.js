export async function compileErlang(code) {
  const res = await fetch("http://localhost:8080/api/erlang/compile", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({"erlang_payload" : { "code" : code }}),
  });
  console.log("Compiling Erlang code:", code);
  if (!res.ok) throw new Error("Error al compilar el código");
  return await res.json();
}

export async function testErlang(code,exercise_id) {
  const token = localStorage.getItem('token');
  const res = await fetch(`http://localhost:8080/api/exercise/${exercise_id}/submit`, {
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