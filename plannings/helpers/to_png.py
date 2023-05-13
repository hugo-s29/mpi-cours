from pdf2image import convert_from_path
import sys

file_name = sys.argv[1]
out_name = file_name[:-3] + 'png'

print(file_name)
print(out_name)

image = convert_from_path(file_name, dpi=500)[0]
image.save(out_name, 'PNG')

