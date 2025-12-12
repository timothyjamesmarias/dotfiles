#include <stdio.h>
#include <stdlib.h>

typedef struct {{CLASS_NAME}} {
    // TODO: Add fields
} {{CLASS_NAME}};

{{CLASS_NAME}}* {{CLASS_NAME}}_new() {
    {{CLASS_NAME}}* obj = malloc(sizeof({{CLASS_NAME}}));
    if (obj == NULL) {
        return NULL;
    }
    // TODO: Initialize fields
    return obj;
}

void {{CLASS_NAME}}_free({{CLASS_NAME}}* obj) {
    if (obj != NULL) {
        // TODO: Free resources
        free(obj);
    }
}
