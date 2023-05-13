import csv, os

def get_content(path):
    with open(path) as f:
        csv_file = csv.reader(f)
        return [ row for row in csv_file ]

files = [ f for f in os.listdir('.') if f.endswith('.csv') ]
content = [ row[4:] for f in files for row in get_content(f)[1:] ]
content = list(zip(*content))

for week in range(20):
    for group in range(1, 16):
        week_list = content[week]
        if not str(group) in week_list:
            print(f'Missing group {group} on week {week+1}')

print('If no error above, OK')
