import os, re, json

def main():
    file_regexp = r'\\input{(.+?\.tex)}'
    tree_regexp = re.compile(r'\\begin{prooftree}((.|\n|\r\n)*?)\\end{prooftree}', re.MULTILINE)

    cwd = os.getcwd()
    main_content = get_file_content(f'{cwd}/main.tex')
    files = sorted(re.findall(file_regexp, main_content))

    for file in files:
        if 'preamble' in file:
            continue

        print('File ' + file)

        file_content = get_file_content(f'{cwd}/{file}')
        src_trees = re.findall(tree_regexp, file_content)
        out = ''
        i = 1
        vars = set()
        for src_tree, _ in src_trees:
            if 'hypo' in src_tree or 'NOCHECK' in src_tree:
                continue

            print('...' + str(i))

            tree = create_tree(src_tree)
            out_tree = coq_verify(tree, i, vars)
            out += out_tree + '\n\n\n'
            i += 1
        if len(src_trees) > 0:
            with open(file + '.proof.v', 'w') as f:
                f.write(out)

def get_file_content(fn):
    with open(fn) as f:
        return f.read()

def create_tree(src):
    instructions = parse_infer(src)
    if len(instructions) == 0:
        return None
    
    def create_from_instructions(parent, instructions):
        if len(instructions) == 0:
            return None

        data = instructions.pop()
        node = ProofTree(*data)
        node.parent = parent

        for i in range(node.ary):
            node.childs[i] = create_from_instructions(node, instructions)

        return node

    tree = create_from_instructions(None, instructions)
    return tree

def parse_infer(src):
    infer_regexp = re.compile(r'\\infer\s*?(\d)(\[.+?\]){(.*)}[\s\t]*(% (.*?))?$', re.MULTILINE)
    infers = re.findall(infer_regexp, src)
    return [
        (hyp, fml, int(arity), rule_name, comment)
        for arity, rule_name, src_sequent, _, comment in infers
        for hyp, fml in [parse_sequent(src_sequent)]
    ]

def parse_sequent(src):
    src_hyp, src_fml = re.findall(r'(.*)\\vdash(.*)', src)[0]
    data = src.split('\\vdash')
    if len(data) == 1:
        src_hyp, src_fml = '\\O', data[0]
    else:
        src_hyp, src_fml = data
    hyp = parse_hypotheses(src_hyp)
    fml = parse_formula(src_fml)
    return (hyp, fml)

def parse_hypotheses(src_hyp):
    src_hyp = src_hyp.strip()
    if src_hyp == '\\O':
        return []

    l = list(map(parse_formula, src_hyp.split(',')))
    if len(l) == 1 and len(l[0].parts) == 0:
        return []
    else:
        return l

operators_regexp = re.compile(r'', re.MULTILINE)
def parse_formula(src):
    src = src.strip()

    if src.startswith('(') and src.endswith(')'):
        new_src = src[1:-1]
        # check before removing outer parentheses
        parentheses = 0
        for c in new_src:
            if parentheses < 0:
                break
            
            if c == '(': parentheses += 1
            elif c == ')': parentheses -= 1

        if parentheses == 0:
            src = new_src

    src = re.sub(r'\\big|\\Big|\\bigg|\\Bigg|\\{|\\}', '', src)
    for op in ['\\land', '\\lor', '\\to', '\\leftrightarrow']:
        src = src.replace(op, '@@' + op + '@@')
    # merge parts based on parentheses
    parts = ['']
    parentheses = 0
    for part in src.split('@@'):
        parentheses += part.count('(')
        parentheses -= part.count(')')
        if parentheses == 0:
            parts[-1] += part
            parts.append('')
        else:
            parts[-1] += part

    parts = [ p.strip() for p in parts if p.strip() != '' ]

    if len(parts) % 2 == 0:
        return Formula([], '')

    operators = set(parts[1::2])

    if len(operators) == 0:
        if '\\lnot' in src:
            part = parts[0].replace('\\lnot', '', 1)
            return Formula([ parse_formula(part) ], '\\lnot')
        elif src == '\\top' or src == '\\bot':
            return Formula([], src)
        else:
            return Formula(parts, '')
    elif len(operators) == 1:
        op = list(operators)[0]
        return Formula([ parse_formula(part) for part in parts[0::2] ], op)
    else:
        return Formula([], '')



class Formula():
    def __init__(self, parts, operator):
        self.parts = parts
        self.operator = operator.strip()

    def __str__(self):
        if len(self.parts) == 0:
            return self.operator
        elif len(self.parts) == 1:
            if self.operator  == '':
                return str(self.parts[0])
            else:
                return self.operator + '(' + str(self.parts[0]) + ')'
        else:
            return '(' + self.operator.join(map(str, self.parts)) + ')'

    def __repr__(self):
        return str(self)

    def pretty_print(self, level = 0):
        if self.operator == '':
            p = self.parts[0]
            if isinstance(p, Formula):
                p.pretty_print(level)
            else:
                print('..' * level + p)
            return

        s = '..' * level
        print(s + self.operator)
        for p in self.parts:
            if isinstance(p, Formula):
                p.pretty_print(level + 1)
            else:
                print(s + '..' + p)

class ProofTree():
    def __init__(self, hyp, fml, ary, rule, comment):
        self.hyp = hyp
        self.fml = fml
        self.ary= ary
        self.rule = rule[1:-1]
        self.childs = [None] * ary
        self.comment = comment
        self.parent = None

    def __str__(self):
        hyps = ','.join(map(str, self.hyp))
        childs = ''
        for c in self.childs:
            str_c = str(c).replace('\n', '\n    ')
            childs += '\n+-->' + str_c + ''
        return f'{self.rule}({self.comment}/{self.ary}) : {hyps} |- {self.fml}{childs}'

    def __repr__(self):
        return str(self)


def vars(f, out = set()):
    for pt in f.parts:
        if isinstance(pt, Formula):
            vars(pt, out)
        else:
            out.add(pt)
    return out

def t_vars(t, out = set()):
    for h in t.hyp:
        vars(h, out)
    vars(t.fml, out)
    for c in t.childs:
        t_vars(c, out)
    return out

def parts(f):
    if isinstance(f, str):
        return f
    return (f.operator, [ parts(s) for s in f.parts ])

def coq_format_fml(f):
    if isinstance(f, str):
        return f

    op_dict = {
        '\\land' : '/\\', '\\lor': '\\/',
        '\\lnot': '~', '\\to' : '->', '': '',
        '\\bot': 'False', '\\Top': 'True'
    }

    def oper(key):
        if key not in op_dict:
            raise Exception('Unknown key "' + key + '" in formula ' + str(f))
        return op_dict[key]

    if len(f.parts) == 0:
        return oper(f.operator)
    elif len(f.parts) == 1:
        return '(' + oper(f.operator) + coq_format_fml(f.parts[0]) + ')'
    else:
        return '(' + oper(f.operator).join(map(coq_format_fml, f.parts)) + ')'


def coq_verify(tree, i, defined_vars):
    vars = t_vars(tree).difference(defined_vars)
    for v in vars:
        defined_vars.add(v)
    root = tree
    out = ''
    if len(vars) > 0:
        out += 'Variables ' + ' '.join(vars) + ' : Prop.\n\n'
    out += f'Theorem test{i} : '
    if len(root.hyp) > 0:
        out += ' -> '.join(map(coq_format_fml, root.hyp))
        out += ' -> '
    out += coq_format_fml(root.fml)
    out += '.\n'
    out += 'Proof.\n'
    if len(root.hyp) > 0:
        out += 'intros ' + ' '.join([ f'H{i+1}' for i in range(len(root.hyp)) ]) + '.\n'
    out += coq_tree(tree, vars)
    out += 'Qed.\n'
    return out

def coq_tree(tree, vars, used_vars = []):
    out = ''
    rev = 'NOREVERSE' not in tree.comment
    tree.comment = tree.comment.replace(' NOREVERSE', '')
    if tree.rule == '$\\lor$i,g':
        out += 'left.\n'
    elif tree.rule == '$\\lor$i,d':
        out += 'right.\n'
    elif tree.rule == '$\\land$i':
        out += 'split.\n'
    elif tree.rule == 'Ax' or tree.rule == '$\\top$i':
        out += 'assumption.\n'
    elif tree.rule == '$\\lnot$i' or tree.rule == '$\\to$i':
        out += 'intro ' + tree.comment + '.\n'
    elif tree.rule == '$\\bot$e':
        out += 'exfalso.\n'
    elif tree.rule == '$\\lnot$e' or tree.rule == '$\\to$e':
        out += 'cut (' + tree.comment + ').\n'
    elif tree.rule == '$\\land$e,g' or tree.rule == '$\\land$e,d':
        a, b, c = [ s.strip() for s in tree.comment.split(' ') if len(s.strip()) > 0 ]
        out += f'destruct {a} as [{b} {c}].\n'
    elif tree.rule == '$\\lor$e':
        a, b, c = [ s.strip() for s in tree.comment.split(' ') if len(s.strip()) > 0 ]
        v = random_avail_var(used_vars, vars)
        used_vars.append(v)
        out += f'cut ({a}). intro {v}. destruct {v} as [{b}|{c}].\n'
        assert(len(tree.childs) == 3)
        out += coq_tree(tree.childs[1], vars, used_vars)
        out += coq_tree(tree.childs[0], vars, used_vars)
        out += coq_tree(tree.childs[2], vars, used_vars)
        return out
    else:
        raise Exception('No rule ' + tree.rule)

    if rev:
        for c in reversed(tree.childs):
            out += coq_tree(c, vars, used_vars)
    else:
        for c in tree.childs:
            out += coq_tree(c, vars, used_vars)

    return out


def random_avail_var(used, vars):
    poss = set('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ')
    for v in used:
        if v in poss:
            poss.remove(v)
    for v in vars:
        if v in poss:
            poss.remove(v)

    if len(poss) == 0:
        raise Exception('No more available variables')

    return poss.pop()

main()
