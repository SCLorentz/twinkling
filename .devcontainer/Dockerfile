# Build stage
FROM debian:bullseye-slim

RUN apt-get update && \
    apt-get -y install sbcl curl git gcc pkg-config

RUN curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --no-sysinit --no-userinit --load /tmp/ql.lisp \
    --eval '(quicklisp-quickstart:install :path "~/.quicklisp")' \
    --eval '(ql:add-to-init-file)' \
    --quit

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y

COPY cmd/.bash /bash

COPY cmd/run.sh /
RUN chmod +x /run.sh

# create bashrc
RUN touch ~/.bashrc && chmod u+w ~/.bashrc
# customize bashrc
COPY cmd/.bash /
RUN echo '. /.bash' >> ~/.bashrc

ENV FLASK_APP=lisp-test

WORKDIR /app
EXPOSE 8000

CMD ["/bin/bash", "-c", "flask run --host 0.0.0.0 --port 8000"]