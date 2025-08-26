# ==========================================
# TEMPLATE MAKEFILE UNIVERSALE per C
# Autore: C4V4H
# Data: 2025-06-08
# ==========================================

# ⚙️  CONFIGURAZIONE PROGETTO - MODIFICA SOLO QUI! ⚙️
PROJECT_NAME = mixAlternate
TARGET = main
MAIN_SOURCE = main.c
EXTRA_SOURCES = mixAlternate.c

# ==========================================
# CONFIGURAZIONE AVANZATA (opzionale)
# ==========================================
CC = gcc
CFLAGS = -Wall -Wextra -std=c99 -g
LDFLAGS = 
DEBUG_FLAGS = -DDEBUG
RELEASE_FLAGS = -O2 -DNDEBUG

# ==========================================
# GENERAZIONE AUTOMATICA (NON MODIFICARE)
# ==========================================
ALL_SOURCES = $(MAIN_SOURCE) $(EXTRA_SOURCES)
HEADERS = $(PROJECT_NAME).h
OBJS = $(ALL_SOURCES:.c=.o)

# ==========================================
# TARGET PRINCIPALI
# ==========================================

# Default target
all: $(TARGET)

# Target principale
$(TARGET): $(OBJS)
	@echo "🔗 Linking $(TARGET)..."
	$(CC) $(OBJS) -o $(TARGET) $(LDFLAGS)
	@echo "✅ Build completato: $(TARGET)"

# Compilazione automatica
%.o: %.c
	@echo "🔨 Compilando $<..."
	$(CC) $(CFLAGS) -c $< -o $@

# ==========================================
# COMANDI ESECUZIONE
# ==========================================

# Esegui il programma
start: $(TARGET)
	@echo "▶️  Eseguendo $(TARGET)..."
	@echo "=========================================="
	./$(TARGET)

# Compila ed esegui
run: $(TARGET) start

# Quick run (clean + build + run)
qrun: clean run

# ==========================================
# MAINTENANCE
# ==========================================

# Pulizia
clean:
	@echo "🧹 Pulizia files temporanei..."
	rm -f $(OBJS) $(TARGET)
	@echo "✅ Pulizia completata"

# Rebuild completo
rebuild: clean all

# ==========================================
# UTILITY
# ==========================================

# Targets che non sono file
.PHONY: all clean start run qrun rebuild memcheck help

# Aiuto
help:
	@echo "=== MAKEFILE HELP - $(PROJECT_NAME) ==="
	@echo ""
	@echo "🏗️  BUILD:"
	@echo "  make / make all  - Compila il progetto"
	@echo "  make rebuild     - Pulisce e ricompila"
	@echo ""
	@echo "▶️  ESECUZIONE:"
	@echo "  make start       - Esegue il programma"
	@echo "  make run         - Compila ed esegue"
	@echo "  make qrun        - Quick run (clean+build+run)"
	@echo ""
	@echo "🛠️  UTILITY:"
	@echo "  make clean       - Rimuove file temporanei"
	@echo "  make help        - Mostra questo aiuto"
