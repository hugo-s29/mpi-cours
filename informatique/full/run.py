import os, json, unicodeit, re, tempfile

data = { "chapters": [], "tds": [], "tps": [], "appendices": [] }

chapters = sorted([ c for c in os.listdir('../cours/') if c.startswith('chap') ])
appendices = sorted([ c for c in os.listdir('../cours/') if c.startswith('annexe') ])
tds = sorted([ c for c in os.listdir('../td/') if c.startswith('td') ])
tps = sorted([ c for c in os.listdir('../tps/') if c.startswith('tp') ])

def detex(txt):
    txt = unicodeit.replace(txt)
    txt = txt.replace('\\textbf{', '')
    txt = txt.replace('\\textsc{', '')
    txt = txt.replace('}', '')
    txt = txt.replace('$', '')
    return txt

def get_title(file):
    with open(file) as f:
        content = f.readlines()
    title_line = [ l for l in content if 'title' in l ][0]
    start = title_line.index('}{') + 2
    end = len(title_line) - 2
    title = title_line[start:end]\
            .replace('\\sc', '\\scshape')\
            .replace('\\bf', '\\bfseries')\
            .replace('\\rm-}', '}-')\
            .replace('{\\bfseries ', '\\textbf{')\
            .replace('{\\scshape ', '\\textsc{')
    return title, content

def find_files(folder, prefix=''):
    files = os.listdir(folder)
    return sorted([
        f'{folder}{file}'
        for file in files
        if file.startswith(prefix) and file.endswith('.tex')
    ])

out =  ''

out += '''%==============================================================
% Fichier généré automatiquement par `run.py,\' ne pas modifier
%==============================================================\n\n'''

out += '\\part{Cours}\n\n'

for chap in chapters:
    out += '{\n'
    chap_num = int(chap[4:])
    chap_name, content = get_title(f'../cours/{chap}/main.tex')
    out += '\t\\chap[' + str(chap_num) + ']{' + chap_name + '}\n'
    out += '\t\\minitoc\n'
    out += '\t\\renewcommand{\cwd}{../cours/' + chap + '/}\n'
    out += '\t\\addmacros{\n'
    exact = '%% EXACT' in ''.join(content)
    data['chapters'].append({ 'name': detex(chap_name), 'id': chap_num })

    if exact:
        begin_docu = [ i for i, l in enumerate(content) if 'begin{document}' in l ][0]
        end_docu = [ i for i, l in enumerate(content) if 'end{document}' in l ][0]
        for line in content[begin_docu + 1 : end_docu]:
            out += '\t' + line.replace('input{', 'input{' + f'../cours/{chap}/')
    else:
        files = find_files(f'../cours/{chap}/', prefix='l')
        for file in files:
            out += '\t\t\\input{' + file + '}\n'

        if 'addrecap' in ''.join(content):
            out += '\t\t\\addrecap\n'

        appendix = find_files(f'../cours/{chap}/', prefix='a')

        if len(appendix) == 0:
            out += '\t}\n'
            out += '\t\\def\\addmacros#1{#1}\n'
            out += '}\n\n'
            continue

        out += '\t\t\\clearpage\n'
        out += '\t\t\\setcounter{section}{0}'
        out += '\t\t\\renewcommand{\\thesection}{\\llap{Annexe }\\thechapter.\\Alph{section}}\n'
        out += '\t\t\\renewcommand{\\thesectionnum}{\\Alph{section}}\n'

        for file in appendix:
            out += '\t\t\\input{' + file + '}\n'

    out += '\t}\n'
    out += '\t\\def\\addmacros#1{#1}\n'
    out += '}\n\n'

out += '\n\n'
out += '\\part{Travaux Dirigés}\n'
out += '\\def\\prefix{\\textsc{td}}\n'
out += '\\renewcommand{\\chaptername}{Travaux Dirigés}\n'
out += '\n\n'

for td in tds:
    try:
        td_num = str(int(td[2:]))
        bonus = False
    except:
        td_num = str(int(td[4:]))
        out += '\\def\\prefix{\\textsc{td bonus}}\n'
        bonus = True
    out += '{\n'
    td_name, content = get_title(f'../td/{td}/main.tex')
    out += '\t\\td[' + td_num + ']{' + td_name + '}\n'
    out += '\t\\minitoc\n'
    out += '\t\\renewcommand{\cwd}{../td/' + td + '/}\n'
    out += '\t\\addmacros{\n'
    exact = '%% EXACT' in ''.join(content)
    data['tds'].append({ 'name': detex(td_name), 'id': td_num, 'bonus': bonus })

    if exact:
        begin_docu = [ i for i, l in enumerate(content) if 'begin{document}' in l ][0]
        end_docu = [ i for i, l in enumerate(content) if 'end{document}' in l ][0]
        for line in content[begin_docu + 1 : end_docu]:
            out += '\t' + line.replace('input{', 'input{' + f'../td/{td}/')
    else:
        files = find_files(f'../td/{td}/', prefix='ex')
        for file in files:
            out += '\t\t\\input{' + file + '}\n'
    out += '\t}\n'
    out += '\t\\def\\addmacros#1{#1}\n'
    out += '}\n'

out += '\n\n'
out += '\\part{Travaux Pratiques}\n'
out += '\\def\\prefix{\\textsc{tp}}\n'
out += '\\renewcommand{\\chaptername}{Travaux pratiques}\n'
out += '\n\n'

for tp in tps:
    try:
        tp_num = str(int(tp[2:]))
        bonus = False
    except:
        tp_num = str(int(tp[4:]))
        out += '\\def\\prefix{\\textsc{tp bonus}}\n'
        bonus = True
    out += '{\n'

    try:
        tp_name, content = get_title(f'../tps/{tp}/main.tex')
        out += '\t\\tp[' + tp_num + ']{' + tp_name + '}\n'
        out += '\t\\minitoc\n'
        out += '\t\\renewcommand{\cwd}{../tps/' + tp + '/}\n'
        out += '\t\\addmacros{\n'
        exact = '%% EXACT' in ''.join(content)
        data['tps'].append({ 'name': detex(tp_name), 'id': tp_num, 'bonus': bonus })

        if exact:
            begin_docu = [ i for i, l in enumerate(content) if 'begin{document}' in l ][0]
            end_docu = [ i for i, l in enumerate(content) if 'end{document}' in l ][0]
            for line in content[begin_docu + 1 : end_docu]:
                out += '\t' + line.replace('input{', 'input{' + f'../tps/{tp}/')
        else:
            files = find_files(f'../tps/{tp}/', prefix='ex')
            for file in files:
                out += '\t\t\\input{' + file + '}\n'
        out += '\t}\n'
    except:
        out += f'\t% No `main.tex\' file for {tp}\n'
    out += '\t\\def\\addmacros#1{#1}\n'
    out += '}\n'

out += '\n\n'
out += '\\part{Annexes}\n'
out += '\\def\\prefix{Annexe}\n'
out += '\\renewcommand{\\chaptername}{Annexe}\n'
out += '\\useroman\n'
out += '\n\n'


for appendix in appendices:
    out += '{\n'
    appendix_num = int(ord(appendix[6:]) - ord('A') + 1)
    try:
        appendix_name, content = get_title(f'../cours/{appendix}/main.tex')
        out += '\t\\chap[' + str(appendix_num) + ']{' + appendix_name + '}\n'
        out += '\t\\minitoc\n'
        out += '\t\\renewcommand{\cwd}{../cours/' + appendix + '/}\n'
        out += '\t\\addmacros{\n'
        exact = '%% EXACT' in ''.join(content)

        data['appendices'].append({ 'name': detex(appendix_name), 'id': appendix_num })

        if exact:
            begin_docu = [ i for i, l in enumerate(content) if 'begin{document}' in l ][0]
            end_docu = [ i for i, l in enumerate(content) if 'end{document}' in l ][0]
            for line in content[begin_docu + 1 : end_docu]:
                out += '\t' + line.replace('input{', 'input{' + f'../cours/{appendix}/')
        else:
            files = find_files(f'../cours/{appendix}/', prefix='l')
            for file in files:
                out += '\t\t\\input{' + file + '}\n'
        out += '\t}\n'
    except:
        out += f'\t% No `main.tex\' file for {appendix}\n'
    out += '\t\\def\\addmacros#1{#1}\n'
    out += '}\n'

with open('generated.tex', 'w') as f:
    f.write(out)

with open('data.json', 'w') as f:
    json.dump(data, f, ensure_ascii=False)
