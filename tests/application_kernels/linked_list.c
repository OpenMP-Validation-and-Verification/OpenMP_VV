/* This test creates a linked list, maps it to a device 
 * (if available) and modidifies the data on the device. 
 * The data is mapped back and contents are verified 
 * 
 * Last modified by Swaroop Pophale, October 2, 2017
 * /

#include <stdlib.h>
#include <omp.h>
#include <stdio.h>

typedef struct node {
    int data;
    struct node *next;
} node_t;

int  map_ll(node_t * head) {
    int i, isHost = -1, exeW[2]={-1,-1};
 //   printf("Entering map_ll\n");

    node_t * temp = head;
    if (!temp) return(2);

    #pragma omp target enter data map(to:temp[:1])
    #pragma omp target map(tofrom: isHost, exeW)
    {
      isHost = omp_is_initial_device();
      exeW[0] = (isHost == 0)? 1 : 0; // 1 = device, 0 = host
      temp->data += 1;  
    }
    isHost=-1;
    while(temp->next) {
        //version 1
        // Note: using array dereference syntax, array section on leaf only
        // Attachment is *not* explicitly guaranteed
        //#pragma omp target enter data map(to:head[0].next[:1])
        //version 2
        //user does an explicit attachment, this is unequivocally correct code
        //but rather slower and nastier.
        node_t * cur = temp->next;
        #pragma omp target enter data map(to:cur[:1])
        #pragma omp target map(tofrom: isHost, exeW)
        {
            isHost = omp_is_initial_device();
            exeW[1] = (isHost == 0)? 1 : 0; // 1 = device, 0 = host
            cur->data += 1;
            temp->next = cur;
        }
        temp=temp->next;
    }
    //printf("Executed on %s\n",(exeW[0] && exeW[1])? "device":"host");
    return((exeW[0] && exeW[1])? 1:0);
}
void unmap_ll(node_t * head) {
    node_t * temp = head;
    if (!temp) return;

    #pragma omp target exit data map(from:temp[0].data)
    while(temp->next) {
        // Note: only copies back the data element to avoid overwriting next
        // pointer
        #pragma omp target exit data map(from:temp[0].next[0].data)
        temp=temp->next;
    }
    //printf("Leaving unmap_ll\n");
}
void push(node_t * head, int data) {
    //printf("Entering push\n");
    node_t * current = head;
    while (current->next != NULL) {
        current = current->next;
    }

    /* now we can add a new variable */
    current->next = (node_t *) malloc(sizeof(node_t));
    current->next->data = data;
    current->next->next = NULL;
    //printf("Leaving push\n");
}

void display(node_t * head)
{
    //printf("Entering display\n");
    node_t * temp=head;
    while(temp!=NULL)
    {
    printf("%d\n",temp->data);
    temp=temp->next;
    }
    //printf("Leaving display\n");
}

int check(node_t * head)
{
    //printf("Entering check\n");
    int error = 0, i=0;
    node_t * temp=head;
    while(temp!=NULL)
    {
      if(temp->data != i+1)
        error=1;
      i++;  
      temp=temp->next;
    }
   return(error);
}

int main() {
    int i, exeW, error=-1;
    node_t * head = NULL;
    head = (node_t *)malloc(sizeof(node_t));
    if (head == NULL) {
        return 1;
    }
    
    head->data = 0;
    head->next = NULL;
    
    for(i=1;i<10;i++)
      push(head,i);

    //display(head);

    exeW = map_ll(head);
    if(exeW == 2)
      printf("ERROR: Head is NULL. Test not executed.\n");
    unmap_ll(head);
    if(check(head))
      printf("Test failed on %s\n",exeW > 0? "device":"host");
    else
      printf("Test passed on %s\n",exeW > 0? "device":"host");
    

    //display(head);

return 0;
}

