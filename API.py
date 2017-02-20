# -*- coding: utf-8 -*-
import html

from flask import Flask, request, send_from_directory
from markdown import markdown

from subject import Subject

app = Flask(__name__)


@app.route('/<path:path>')
def send_static(path):
    return send_from_directory('static', path)


@app.route('/synthesis', methods=['POST'])
def getSynthesis():
    # try:
    subject = Subject(request.form['subject'])
    # except Exception as e:
    #     return '<h1>Error</h1>' + str(e)
    subjectText = html.escape(str(subject), quote=False).replace(
        '\\', '')  # Remove extra escapes, prep it for HTML
    return "<script>alert('Your synthesis is done!')</script>" + markdown(subjectText)
