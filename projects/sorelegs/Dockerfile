FROM python:3.11-slim

RUN mkdir /app
WORKDIR /app

RUN apt update && apt install -y uwsgi-plugin-python3
RUN python -m pip install -U pip poetry gunicorn

COPY poetry.lock pyproject.toml ./
RUN poetry config virtualenvs.create false \
    && poetry install --no-interaction && \
    rm -rf ~/.config/pypoetry ~/.cache/pypoetry

COPY . /app

ENV SQLITE_DB_PATH="/data/db.sqlite3"
ENV MEDIA_STORAGE_PATH="/data/media/"

ENTRYPOINT [ "/app/entrypoint.sh" ]
EXPOSE 8080
