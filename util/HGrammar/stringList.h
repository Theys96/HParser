
typedef struct StringList {
	int size;
	char** list;
} StringList;

typedef struct StringListList {
	int size;
	char*** list;
} StringListList;

StringList newStringList();
StringList* insertStringList(StringList* sl, char* string);
void freeStringList(StringList* sl);
void printStringList(StringList* sl);

StringListList newStringListList();
StringListList* consumeStringList(StringListList* sll, StringList* sl);
void printStringListList(StringListList* sll);
void freeStringListList(StringListList* sll);
