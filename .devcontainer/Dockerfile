# Build stage
FROM debian:bullseye-slim

RUN apt-get update && \
    apt-get install -y build-essential git curl sbcl \
    libgtk-3-dev libcairo2-dev libjpeg-dev libgif-dev \
    libglib2.0-dev libpango1.0-dev

#build-essential libgtk-3-dev

RUN curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --no-sysinit --no-userinit --load /tmp/ql.lisp \
    --eval '(quicklisp-quickstart:install :path "~/.quicklisp")' \
    --eval '(ql:add-to-init-file)' \
    --quit

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y

COPY cmd/.bash /bash

# create bashrc
RUN touch ~/.bashrc && chmod u+w ~/.bashrc
# customize bashrc
COPY cmd/.bash /
RUN echo '. /.bash' >> ~/.bashrc

ENV FLASK_APP=lisp-test

WORKDIR /app
EXPOSE 8000

CMD ["/bin/bash", "-c", "flask run --host 0.0.0.0 --port 8000"]