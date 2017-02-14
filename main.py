from flask import Flask, request, send_from_directory
from markdown import markdown

from subject import Subject

app = Flask(__name__)

@app.route('/<path:path>')
def send_static(path):
    return send_from_directory('static', path)

@app.route('/synthesis', methods=['POST'])
def getSynthesis():
    print('hit')
    subject = Subject(request.form['subject'])
    return markdown(str(subject))