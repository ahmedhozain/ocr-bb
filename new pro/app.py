import os
from flask import Flask, render_template, request
from PIL import Image
import pytesseract
import re
import clips

app = Flask(__name__)
UPLOAD_FOLDER = 'static/uploads'
app.config['UPLOAD_FOLDER'] = UPLOAD_FOLDER

pytesseract.pytesseract.tesseract_cmd = r'C:\Program Files\Tesseract-OCR\tesseract.exe'  # تأكد من المسار

@app.route('/', methods=['GET', 'POST'])
def index():
    if request.method == 'POST':
        file = request.files['image']
        if file:
            filepath = os.path.join(app.config['UPLOAD_FOLDER'], file.filename)
            file.save(filepath)
            text = extract_text(filepath)
            values = extract_values(text)
            analysis = run_clips(values)
            return render_template('index.html', extracted_text=text, analysis=analysis, filename=file.filename)
    return render_template('index.html')

def extract_text(image_path):
    img = Image.open(image_path)
    return pytesseract.image_to_string(img, lang='eng')

def extract_values(text):
    data = {
        "pH": re.search(r"pH\s*:\s*(\d+\.?\d*)", text),
        "specific_gravity": re.search(r"Specific Gravity\s*:\s*(\d+\.?\d*)", text),
        "pus_cells": re.search(r"Pus Cells\s*:\s*([\w\-]+)", text),
        "rbcs": re.search(r"R\.?B\.?C\.?s\s*:\s*([\w\-]+)", text),
        "crystals": re.search(r"Crystals\s*:\s*(\w+\s?\+*|Absent)", text),
    }
    return {k: (v.group(1) if v else "Unknown") for k, v in data.items()}

def run_clips(values):
    env = clips.Environment()
    env.load("rules.clp")

    fact = env.assert_string(f"""(result 
        (pH {values['pH']}) 
        (specific_gravity {values['specific_gravity']}) 
        (pus_cells {values['pus_cells']}) 
        (rbcs {values['rbcs']}) 
        (crystals {values['crystals']})
    )""")

    env.run()
    output = "\n".join(str(m) for m in env.get_output())
    return output or "✅ All results are within normal range."

if __name__ == '__main__':
    app.run(debug=True)
