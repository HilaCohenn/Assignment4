//Q1
export function all<T>(promises : Array<Promise<T>>) : Promise<Array<T>> {
  return new Promise<T[]>((resolve, reject) => {
    const results: T[] = [];
    let completed = 0;

    if (promises.length === 0) {
      resolve([]);
      return;
    }

    promises.forEach((promise, i) => {
      promise.then(
        value => {
          results[i] = value;
          completed++;
          if (completed === promises.length) {
            resolve(results);
          }
        },
        err => {
          reject(err);
        }
      );
    });
  });
}

  
// Q2
export function* Fib1() {
  // Recurrence relation: F1 = 1, F2 = 1, Fn = Fn-1 + Fn-2
  let a = 1, b = 1;
  yield a; // F1
  yield b; // F2
  while (true) {
    const next = a + b;
    yield next;
    a = b;
    b = next;
  }
}

export function* Fib2() {
  // Binet's formula: F(n) = (phi^n - psi^n) / sqrt(5)
  const sqrt5 = Math.sqrt(5);
  const phi = (1 + sqrt5) / 2;
  const psi = (1 - sqrt5) / 2;
  let n = 1;
  while (true) {
    // Round to nearest integer to avoid floating point errors
    yield Math.round((Math.pow(phi, n) - Math.pow(psi, n)) / sqrt5);
    n++;
  }
}
