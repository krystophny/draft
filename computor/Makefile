# Load environment variables
ifneq (,$(wildcard .env))
	include .env
	export
endif

DOCKER_COMPOSE = docker-compose
PREFECT_API_URL ?= http://localhost:4200/api

.PHONY: up down restart clean db logs build \
        python-agent matlab-agent system-agent \
        wait-for-db deploy-prefect all-agents

up:
	docker-compose up -d

down:
	docker-compose down

restart: down up

clean:
	docker-compose down -v

build:
	docker-compose build

logs:
	docker-compose logs -f

wait-for-db:
	@echo "⏳ Waiting for PostgreSQL to become ready..."
	@until docker exec $$(docker ps -qf "name=postgres") pg_isready -U $$POSTGRES_USER -d $$POSTGRES_DB > /dev/null 2>&1; do \
		sleep 1; \
	done
	@echo "✅ PostgreSQL is ready."

db: wait-for-db
	./migrations_up.sh

python-agent:
	./python-agent-dev.sh

matlab-agent:
	docker-compose -f matlab-worker.yaml up -d

system-agent:
	./system-agent-dev.sh

all-agents: python-agent system-agent matlab-agent

# OPTIONAL: Prefect flow deployment scaffold
deploy-prefect:
	@echo "🚀 Deploying Prefect flows..."
	python execution_backend/flows/deploy_all.py

