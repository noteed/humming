FROM ubuntu:12.04

RUN apt-get update
RUN apt-get install -q -y language-pack-en
RUN update-locale LANG=en_US.UTF-8

# For https://github.com/dotcloud/docker/issues/2972
RUN apt-get install -q -y sudo

# Install PostgreSQL
RUN apt-get install -q -y postgresql-9.1
RUN apt-get install -q -y libgmp10 libpq5

# Configure
RUN sh -c "service postgresql start ; su postgres -c 'createuser -d -r -s docker' ; service postgresql stop"
RUN sh -c "service postgresql start ; su postgres -c \"psql -c \\\"ALTER USER docker PASSWORD 'docker'\\\"\" ; service postgresql stop"
RUN sh -c "service postgresql start ; su postgres -c 'createdb -O docker docker' ; service postgresql stop"
RUN sed -i -e 's/^host *all *all *127.0.0.1\/32 *md5$/host  all  all  0.0.0.0\/0  md5/' /etc/postgresql/9.1/main/pg_hba.conf
RUN echo "listen_addresses = '*'" >> /etc/postgresql/9.1/main/postgresql.conf

ADD run.sh /run.sh
ADD humming /usr/bin/humming

CMD /run.sh
