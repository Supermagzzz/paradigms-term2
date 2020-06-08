"use strict";

let variablesIndex = {x: 0, y: 1, z: 2};

const cnst = value => () => value;
const variable = function (name) {
    const index = variablesIndex[name];
    return (...args) => args[index];
};

const operation = f => {
    let result = (...operands) => (...args) => f(...operands.map(operand => operand(...args)));
    result.cntArgs = f.length;
    return result;
};

const add = operation((a, b) => a + b);
const subtract = operation((a, b) => a - b);
const multiply = operation((a, b) => a * b);
const divide = operation((a, b) => a / b);

const negate = operation(x => -x);
const abs = operation(Math.abs);

const iff = operation((a, b, c) => a >= 0 ? b : c);

const one = cnst(1);
const two = cnst(2);

let operations = {
    "one" : () => one, "two": () => two,
    "x": () => variable('x'), "y": () => variable('y'), "z": () => variable('z'),
    "abs": abs, "negate": negate,
    "+": add, "-": subtract, "*": multiply, "/": divide,
    "iff": iff
};

const parse = function(expression) {
    let stack = [];
    for (let token of expression.split(' ').filter(a => a !== "")) {
        if (token in operations) {
            let operation = operations[token];
            let cntOperands = (operation.hasOwnProperty("cntArgs") ? operation.cntArgs : 0);
            stack[stack.length - cntOperands] = operation(...stack.splice(stack.length - cntOperands, cntOperands));
        } else {
            stack.push(cnst(+token));
        }
    }
    return stack.pop();
};

