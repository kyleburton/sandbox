FROM python:3.5.1-alpine

COPY requirements.txt requirements.txt
RUN pip install -r requirements.txt

COPY my-app.py my-app.py
ENTRYPOINT ["python", "my-app.py"]
