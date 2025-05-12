import os
from flask import Flask, render_template, request
from PIL import Image
import pytesseract
import re
import clips # type: ignore

app = Flask(__name__)
UPLOAD_FOLDER = 'static/uploads'
app.config['UPLOAD_FOLDER'] = UPLOAD_FOLDER

# مكان tesseract
pytesseract.pytesseract.tesseract_cmd = r'C:\Program Files\Tesseract-OCR\tesseract.exe'

# استخلاص القيم من النص
def extract_values(text):
    data = {}
    patterns = {
        "pH": r"pH\s*:\s*(\d+\.?\d*)",
        "Specific Gravity": r"Specific Gravity\s*:\s*(\d+\.?\d*)",
        "Pus Cells": r"Pus Cells\s*:\s*(\d+-\d+)",
        "R.B.Cs": r"R\.B\.Cs\s*:\s*(\d+-\d+)",
        "Crystals": r"Crystals\s*:\s*(\w+\s?\+*)"
    }
    for key, pattern in patterns.items():
        match = re.search(pattern, text)
        if match:
            data[key] = match.group(1)
    return data

# تحليل البيانات بقواعد CLIPS
def run_clips_rules(data_dict):
    env = clips.Environment()
    env.load("rules.clp")
    for key, value in data_dict.items():
        fact = env.assert_string(f'(result (name "{key}") (value "{value}"))')
    env.run()
    
    results = []
    for fact in env.facts():
        if "diagnose" in str(fact):
            results.append(str(fact))
    return results

# تحليل الصورة
def process_image(image_path):
    img = Image.open(image_path)
    text = pytesseract.image_to_string(img, lang='eng')
    values = extract_values(text)
    diagnoses = run_clips_rules(values)
    return text, values, diagnoses

@app.route('/', methods=['GET', 'POST'])
def index():
    if request.method == 'POST':
        file = request.files['image']
        if file:
            filepath = os.path.join(app.config['UPLOAD_FOLDER'], file.filename)
            file.save(filepath)
            extracted_text, values, diagnoses = process_image(filepath)
            return render_template('index.html',
                                   extracted_text=extracted_text,
                                   filename=file.filename,
                                   values=values,
                                   diagnoses=diagnoses)
    return render_template('index.html')

if __name__ == '__main__':
    app.run(debug=True)
