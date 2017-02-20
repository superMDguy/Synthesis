# -*- coding: utf-8 -*-
import html
import os

from flask import Flask, request, send_from_directory
from rq import Queue
from worker import conn

from subject import Subject

app = Flask(__name__)
q = Queue(connection=conn)


@app.route('/<path:path>')
def send_static(path):
    return send_from_directory('static', path)


@app.route('/synthesis', methods=['POST'])
def getSynthesis():
    try:
        id_str = request.form['subject'].replace(' ', '')
        subject = Subject(request.form['subject'])
        q.enqueue(subject.build, job_id=id_str)
    except Exception as e:
        return '<h1>Error</h1>' + str(e)
    return 'Started Synthesis at <a href=/status/{0}>/status/{1}</a>'.format(id_str, id_str)


@app.route('/status/<job_id>')
def job_status(job_id):
    job = q.fetch_job(job_id)
    if job is None:
        return 'Not done yet...'
    else:
        subjectText = str(job.result).replace('\\', '')  # Remove extra escapes
        if job.is_failed:
            return '<h1>Error!</h1>'
        return subjectText

if __name__ == "__main__":
    port = int(os.environ.get("PORT", 5000))
    app.run(host='0.0.0.0', port=port)
