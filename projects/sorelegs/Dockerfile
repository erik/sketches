FROM python:3.11-slim

RUN mkdir /app
WORKDIR /app

RUN python -m pip install -U pip poetry

COPY poetry.lock pyproject.toml ./
RUN poetry config virtualenvs.create false \
    && poetry install --no-dev --no-interaction && \
    rm -rf ~/.config/pypoetry ~/.cache/pypoetry 

COPY . /app

ENV SQLITE_DB_PATH="/data/db.sqlite3"

ENTRYPOINT [ "/app/entrypoint.sh" ]
EXPOSE 8080