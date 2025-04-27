import os
from flask import Flask, render_template, request, redirect, url_for
from PIL import Image
import pytesseract
import re
import tempfile


app = Flask(__name__)


pytesseract.pytesseract.tesseract_cmd = r'C:\Program Files\Tesseract-OCR\tesseract.exe'


UPLOAD_FOLDER = 'static/uploads'
app.config['UPLOAD_FOLDER'] = UPLOAD_FOLDER


@app.route('/', methods=['GET', 'POST'])
def index():
    if request.method == 'POST':
        
        file = request.files['image']
        if file:
            
            filename = os.path.join(app.config['UPLOAD_FOLDER'], file.filename)
            file.save(filename)

            
            print(f"Image saved at: {filename}")

            
            extracted_text = process_image(filename)

            
            print(f"Extracted Text: {extracted_text}")

            return render_template('index.html', extracted_text=extracted_text, filename=file.filename)
    
    return render_template('index.html')

# دالة لتحليل الصورة باستخدام Tesseract
def process_image(image_path):
    try:
        # تحميل الصورة
        img = Image.open(image_path)

        # استخراج النص
        extracted_text = pytesseract.image_to_string(img, lang='eng')

        # طباعة للمساعدة في التشخيص
        print(f"Extracted Text from image: {extracted_text}")

        
        def extract_values(text):
            data = {}
            patterns = {
                "pH": r"pH\s*:\s*(\d+\.?\d*)",
                "Specific Gravity": r"Specific Gravity\s*:\s*(\d+)",
                "Pus Cells": r"Pus Cells\s*:\s*(\d+-\d+)",
                "R.B.Cs": r"R\.B\.Cs\s*:\s*(\d+-\d+)",
                "Crystals": r"Crystals\s*:\s*(\w+\s?\+*)"
            }
            for key, pattern in patterns.items():
                match = re.search(pattern, text)
                if match:
                    data[key] = match.group(1)
            return data

        
        result = extract_values(extracted_text)
        return extracted_text
    except Exception as e:
        print(f"Error during image processing: {e}")
        return "Error processing image."

if __name__ == '__main__':
    app.run(debug=True)
