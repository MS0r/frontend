export async function compileErlang(code) {
  const res = await fetch("http://localhost:8080/api/erlang/compile", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({"erlang_payload" : { "source_code" : code }}),
  });
  console.log("Compiling Erlang code:", code);
  if (!res.ok) throw new Error("Error al compilar el código");
  return await res.json();
}

export async function testErlang(code,exercise_id) {
  const res = await fetch(`http://localhost:8080/api/erlang/test/${exercise_id}`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({"erlang_payload" : { "source_code" : code }}),
  });
  if (!res.ok) throw new Error("Error al probar el código");
  return await res.json();
}