function f(a, b) {
    if(typeof a === 'number' && typeof b === 'number') {
        var max = a;
        if(max < b)
            max = b;
        var sum = a+b;
        return {somma: sum, massimo: max};
    } else {
        throw {name:'TypeError' , message: 'type mismatch error'}
    }
}