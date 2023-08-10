# -*- coding: utf-8 -*-
import html
import os
from time import time

from flask import Flask, request, send_from_directory, render_template, make_response
from rq import Queue
from worker import conn

from subject import Subject

app = Flask(__name__)
q = Queue(connection=conn)


@app.route('/')
def send_index():
    return send_from_directory('static', 'index.html')


@app.route('/<path:path>')
def send_static(path):
    return send_from_directory('static', path)


@app.route('/synthesis', methods=['POST'])
def getSynthesis():
    try:
        id_str = (request.form['subject']+str(time())).replace(' ', '-')
        if request.form.get('wikipedia'):
            useWikipedia = True
        else:
            useWikipedia = False
        subject = Subject(request.form['subject'], summaryLength=int(
            request.form['lines']), useWikipedia=useWikipedia)
        q.enqueue(subject.build, job_id=id_str, timeout=600)
    except Exception as e:
        return '<h1>Error</h1>' + str(e)
    return render_template('working.html', url='/status/' + id_str)


@app.route('/status/<job_id>')
def job_status(job_id):
    job = q.fetch_job(job_id)
    if job.result is None:
        response = make_response('no')
    else:
        subjectText = render_template(
            'done.html', sections=job.result.sections, title=job.result.wikiPage.title, sourceMap=job.result.sourceMap)
        if job.is_failed:
            subjectText = '<h1>Error!</h1>'
        response = make_response(subjectText)
    response.headers['Cache-Control'] = 'no-cache, no-store, must-revalidate'
    response.headers['Pragma'] = 'no-cache'
    return response

if __name__ == "__main__":
    port = int(os.environ.get("PORT", 5003))
    app.run(host='0.0.0.0', port=port)
    print('running on localhost:5003')
