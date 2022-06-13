function f(a, b) {
    if(typeof a === 'number' && typeof b === 'number') {
        var fat = 1;
        var i;
        for (i = 1; i++; i < a) {
            fat = fat * i;
        }
        return {fattoriale: fat, prodotto: a*b};
    } else {
        throw {name: 'TypeError', message: 'type mismatch error'}
    }
}