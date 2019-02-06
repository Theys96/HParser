#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "stringList.h"

StringList newStringList() {
	StringList sl;
	sl.size = 0;
	sl.list = calloc((sl.size+1), sizeof(char*));
	sl.list[0] = NULL;
	return sl;
}

StringList* insertStringList(StringList* sl, char* string) {
	sl->size++;
	sl->list = realloc(sl->list, sizeof(char*) * (sl->size+1));
	sl->list[sl->size-1] = calloc(strlen(string)+1, sizeof(char));
	strcpy(sl->list[sl->size-1], string);
	sl->list[sl->size] = NULL;
	return sl;
}

void freeStringList(StringList* sl) {
	for (int i = 0; i < sl->size; i++) {
		free(sl->list[i]);
	}
	free(sl->list);
}

void printStringList(StringList* sl) {
	for (int i = 0; i < sl->size; i++) {
		printf("%2d\t%s\n", i, sl->list[i]);
	}
}

StringListList newStringListList() {
	StringListList sll;
	sll.size = 0;
	sll.list = calloc(0, sizeof(char**));
	return sll;
}

StringListList* consumeStringList(StringListList* sll, StringList* sl) {
	sll->size++;
	sll->list = realloc(sll->list, sizeof(char**) * sll->size);
	int i = sll->size-1;
	sll->list[i] = calloc(sl->size + 1, sizeof(char*));
	for (int s = 0; s < sl->size; s++) {
		sll->list[i][s] = sl->list[s];
	}
	sll->list[i][sl->size] = NULL;
	free(sl->list);
	return sll;
}

void printStringListList(StringListList* sll) {
	for (int i = 0; i < sll->size; i++) {
		int s = 0;
		printf("%2d\n", i);
		while (sll->list[i][s] != NULL) {
			printf("\t%2d\t%s\n", s, sll->list[i][s]);
			s++;
		}
	}
}

void freeStringListList(StringListList* sll) {
	for (int i = 0; i < sll->size; i++) {
		int s = 0;
		while (sll->list[i][s] != NULL) {
			free(sll->list[i][s]);
			s++;
		}
		free(sll->list[i]);
	}
	free(sll->list);
}
