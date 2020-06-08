"use strict";

function ParseException(cursor, token) {
    this.message = this.name + " in position " + cursor + this.getMyMessage(token);
}
ParseException.prototype = Object.create(Error.prototype);

function createException(name, getMyMessage) {
    function Error(cursor, token) {
        ParseException.call(this, cursor, token);
    }
    Error.prototype = Object.create(ParseException.prototype);
    Error.prototype.name = name;
    Error.prototype.getMyMessage = getMyMessage;
    Error.prototype.constructor = ParseException;
    return Error;
}

const UnexpectedToken = createException("Unexpected token", (token) => " token: " + token);
const UnexpectedEnd = createException("Unexpected end", () => "");

let variablesNames = {"x" : 0, "y" : 1, "z" : 2};

function Operation(...args) {
    this.operands = args;
}
Operation.prototype.evaluate = function (...args) {
    return this.f(...this.operands.map(operand => operand.evaluate(...args)));
};
Operation.prototype.diff = function(x) {
    return this.diffFunc(x, ...this.operands);
};
Operation.prototype.toString = function() {
    return this.operands.join(" ") + " " + this.name;
};
Operation.prototype.prefix = function() {
    return "(" + this.name + " " + this.operands.map(t => t.prefix()).join(" ") + ")";
};
Operation.prototype.postfix = function() {
    return "(" + this.operands.map(t => t.postfix()).join(" ") + " " + this.name + ")";
};

const createOperation = (name, f, diffFunc)  => {
    let Constructor = function (...args) {
        Operation.call(this, ...args);
    };
    Constructor.prototype = Object.create(Operation.prototype);
    Constructor.prototype.name = name;
    Constructor.prototype.f = f;
    Constructor.prototype.diffFunc = diffFunc;
    Constructor.operandsCount = x => f.length === 0 ? x : f.length;
    return Constructor;
};

function Const(value) {
    this.value = value;
}
Const.prototype.evaluate = function () {
    return this.value
};
Const.prototype.diff = function() {
    return zero;
};
Const.prototype.toString = Const.prototype.prefix = Const.prototype.postfix = function() {
    return this.value.toString();
};

let zero = new Const(0);
let one = new Const(1);

function Variable(name) {
    this.name = name;
    this.ind = variablesNames[name];
}
Variable.prototype.evaluate = function (...args) {
    return args[this.ind];
};
Variable.prototype.diff = function(x) {
    return this.name === x ? one : zero;
};
Variable.prototype.toString = Variable.prototype.prefix = Variable.prototype.postfix = function() {
    return this.name;
};

const Add = createOperation("+", (a, b) => a + b, (x, a, b) => new Add(a.diff(x), b.diff(x)));
const Subtract = createOperation("-", (a, b) => a - b, (x, a, b) => new Subtract(a.diff(x), b.diff(x)));
const Multiply = createOperation("*", (a, b) => a * b,
    (x, a, b) => new Add(new Multiply(a, b.diff(x)), new Multiply(a.diff(x), b))
);
const Divide = createOperation("/", (a, b) => a / b,
    (x, a, b) => new Divide(
        new Subtract(new Multiply(a.diff(x), b), new Multiply(a, b.diff(x))),
        new Multiply(b, b)
    )
);

const Negate = createOperation("negate", x => -x, (x, a) => new Negate(a.diff(x)));

const Gauss = createOperation("gauss", (a, b, c, x) => a * Math.exp(-(x - b) * (x - b) / (2 * c * c)),
    (x, a, b, c, d) => {
        const c3 = new Multiply(c, new Multiply(c, c));
        const dsb = new Subtract(d, b);
        return new Multiply(
            new Gauss(new Divide(one, c3), b, c, d),
            new Add(
                new Multiply(a.diff(x), c3),
                new Multiply(a, new Multiply(
                    dsb, new Subtract(
                        new Multiply(c.diff(x), dsb),
                        new Multiply(c, dsb.diff(x))
                    )
                ))
            )
        )
    }
);

const Mean = createOperation("mean",
    (...args) => args.reduce((sum, cur) => sum + cur, 0) / args.length,
    (x, ...args) => new Mean(...args.map(t => t.diff(x))));
const Var = createOperation("var",
    (...args) => {
        let a = args.reduce((sum, cur) => sum + cur, 0) / args.length;
        return args.reduce((sum, cur) => sum + (cur - a) * (cur - a), 0) / args.length;
    },
    (x, ...args) => {
        const mean = new Mean(...args);
        return new Mean(...args.map(a => {
            const asm = new Subtract(a, mean);
            return new Multiply(asm, asm).diff(x);
        }));
    });

const operations = {"negate" : Negate, "+" : Add, "-" : Subtract, "*" : Multiply, "/" : Divide, "gauss" : Gauss,
    "mean" : Mean, "var" : Var};

let variables = {"x" : new Variable("x"), "y" : new Variable("y"), "z" : new Variable("z")};

function BaseParser(expression) {
    this.cursor = 0;
    this.expression = expression;
}
BaseParser.prototype.skipWhitespaces = function () {
    while (this.cursor !== this.expression.length && this.expression[this.cursor] === ' ') {
        this.cursor++;
    }
};
BaseParser.prototype.checkEnd = function () {
    this.skipWhitespaces();
    return this.cursor === this.expression.length || this.expression[this.cursor] === ')';
};
BaseParser.prototype.parse = function () {
    let result = this.readOperand();
    this.skipWhitespaces();
    if (this.cursor !== this.expression.length) {
        throw new UnexpectedEnd(this.cursor);
    }
    return result;
};
BaseParser.prototype.readToken = function (f = x => x) {
    if (this.checkEnd()) {
        throw new UnexpectedEnd(this.cursor);
    }
    if (this.expression[this.cursor] === '(') {
        this.cursor++;
        let result = this.readTokens();
        this.skipWhitespaces();
        if (this.expression.length === this.cursor) {
            throw new UnexpectedEnd(this.cursor);
        }
        if (this.expression[this.cursor] !== ')') {
            throw new UnexpectedToken(this.cursor, this.expression[this.cursor]);
        }
        this.cursor++;
        return result;
    }
    let start = this.cursor;
    while (this.cursor !== this.expression.length && this.expression[this.cursor] !== ' ' &&
    this.expression[this.cursor] !== ')' && this.expression[this.cursor] !== '(') {
        this.cursor++;
    }
    return f(this.expression.substr(start, this.cursor - start));
};
BaseParser.prototype.parseOperation = function(token, operands) {
    if (token in operations) {
        operands.forEach(op => {
            if (op in operations) {
                throw new UnexpectedToken(this.cursor, op);
            }
        });
        let operation = operations[token];
        if (operands.length !== operation.operandsCount(operands.length)) {
            throw new UnexpectedToken(this.cursor, token);
        }
        return new operation(...operands);
    }
    throw new UnexpectedToken(this.cursor, token);
};
BaseParser.prototype.parseOperand = function(token) {
    if (token in variables) {
        return variables[token];
    } else if (isFinite(token) && token.length > 0) {
        return new Const(+token);
    } else {
        throw new UnexpectedToken(this.cursor, token);
    }
};
BaseParser.prototype.readOperand = function () {
    return this.readToken(token => this.parseOperand(token));
};
BaseParser.prototype.readAll = function () {
    return this.readToken(token => token in operations ? token : this.parseOperand(token));
};
BaseParser.prototype.readTokens = function () {
    let operands = [];
    while (!this.checkEnd()) {
        operands.push(this.readAll());
    }
    if (operands.length === 0) {
        throw new UnexpectedToken(this.cursor, ')');
    }
    return this.parseOperation(this.getOperation(operands), operands);
};

const createParser = (getOperation) => {
    let Constructor = function (expression) {
        BaseParser.call(this, expression);
    };
    Constructor.prototype = Object.create(BaseParser.prototype);
    Constructor.prototype.getOperation = getOperation;
    return Constructor;
};

const getParser = parser => expression => new parser(expression).parse();

const parsePostfix = getParser(createParser(op => op.pop()));
const parsePrefix = getParser(createParser(op => op.shift()));

function parse(expression) {
    let stack = [];
    for (let token of expression.split(' ').filter(a => a !== "")) {
        if (token in operations) {
            let operation = operations[token];
            let cnt = operation.operandsCount(stack.length);
            stack[stack.length - cnt] = new operation(...stack.splice(stack.length - cnt, cnt));
        } else if (token in variables) {
            stack.push(variables[token]);
        } else if (isFinite(token)) {
            stack.push(new Const(+token));
        } else {
            throw new UnexpectedToken(this.cursor, token);
        }
    }
    return stack.pop();
}