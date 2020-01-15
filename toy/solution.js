function exchange(i, j) {
	let tmp = t[i];
	t[i] = t[j];
	t[j] = tmp;
}

function spin(n) {
	let u = t.slice();
	function f(i) {
		if (i != t.length) {
			u[i] = t[(t.length - n + i) % t.length];
			f(i + 1);
		}
	}
	f(0);
	t = u;
}

function smoke_test() {
	init_mini();
	courte();
	expect("acdb");
}
