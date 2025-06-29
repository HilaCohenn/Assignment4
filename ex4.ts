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
	// @TODO
  return undefined;
}


export function* Fib2() {
	// @TODO
  return undefined;
}
