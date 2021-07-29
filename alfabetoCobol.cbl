ID DIVISION.                                                     
PROGRAM-ID. PATTERN.                                             
AUTHOR. gangagoura.                                              
DATE-WRITTEN. 24-MAR-2021.                                       
DATA DIVISION.                                                   
WORKING-STORAGE SECTION.                                         
01 WS-DATA-ITEMS.                                                
   05 WS-INPUT                     PIC X(10).                    
   05 WS-INPUT-UPPER               PIC X(10).                    
   05 WS-PART1                     PIC X(10).                    
   05 WS-PART2                     PIC X(10).                    
   05 WS-COUNT                     PIC 9(2) COMP.                
   05 WS-I                         PIC 9(2).                     
   05 WS-J                         PIC 9(2).                     
   05 WS-K                         PIC 9(2).                     
   05 WS-L                         PIC 9(2).                     
   05 WS-TEMP                      PIC 9(2).                     
01 WS-TABLE1.                                                    
   05 WS-LINE OCCURS 7 TIMES.                                    
      10 WS-LETTER OCCURS 70 TIMES PIC X(1).                     
01 WS-TABLE2.                                                    
   05 WS-DISPLAY-LINE OCCURS 7 TIMES.                            
      10 WS-DISPLAY OCCURS 10 TIMES.                             
         15 WS-DATA                PIC X(7).                     
         15 WS-FILLER              PIC X.                        
PROCEDURE DIVISION.                                              
    INITIALIZE WS-TABLE1                                         
               WS-TABLE2                                         
               WS-DATA-ITEMS.                                    
    PERFORM ASK-USER THRU ASK-EXIT                               
    PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-COUNT       
    EVALUATE WS-INPUT-UPPER(WS-I:1)                              
    WHEN 'A'                                                     
    PERFORM A-PARA THRU A-EXIT                                   
    WHEN 'B'                                                     
    PERFORM B-PARA THRU B-EXIT                                   
    WHEN 'C'                                                     
    PERFORM C-PARA THRU C-EXIT                                   
    WHEN 'D'                                                     
    PERFORM D-PARA THRU D-EXIT                                   
    WHEN 'E'                                                     
    PERFORM E-PARA THRU E-EXIT                                   
    WHEN 'F'                                                     
    PERFORM F-PARA THRU F-EXIT                                   
    WHEN 'G'                                                     
    PERFORM G-PARA THRU G-EXIT                                   
    WHEN 'H'                                                     
    PERFORM H-PARA THRU H-EXIT                                   
    WHEN 'I'                                                     
    PERFORM I-PARA THRU I-EXIT                                   
    WHEN 'J'                                                     
    PERFORM J-PARA THRU J-EXIT                                   
    WHEN 'K'                                                     
    PERFORM K-PARA THRU K-EXIT                                   
    WHEN 'L'                                                     
    PERFORM L-PARA THRU L-EXIT                                   
    WHEN 'M'                                                     
    PERFORM M-PARA THRU M-EXIT                                   
    WHEN 'N'                                                     
    PERFORM N-PARA THRU N-EXIT                                   
    WHEN 'O'                                                     
    PERFORM O-PARA THRU O-EXIT                                   
    WHEN 'P'                                                     
    PERFORM P-PARA THRU P-EXIT                                   
    WHEN 'Q'                                                     
    PERFORM Q-PARA THRU Q-EXIT                                   
    WHEN 'R'                                                     
    PERFORM R-PARA THRU R-EXIT                                   
    WHEN 'S'                                                     
    PERFORM S-PARA THRU S-EXIT                                   
    WHEN 'T'                                                     
    PERFORM T-PARA THRU T-EXIT                                   
    WHEN 'U'                                                     
    PERFORM U-PARA THRU U-EXIT                                   
    WHEN 'V'                                                     
    PERFORM V-PARA THRU V-EXIT                                   
    WHEN 'W'                                                     
    PERFORM W-PARA THRU W-EXIT                                   
    WHEN 'X'                                                     
    PERFORM X-PARA THRU X-EXIT                                   
    WHEN 'Y'                                                     
    PERFORM Y-PARA THRU Y-EXIT                                   
    WHEN 'Z'                                                     
    PERFORM Z-PARA THRU Z-EXIT                                   
    END-EVALUATE                                                 
    END-PERFORM                                                  
    PERFORM DISPLAY-PARA THRU DISPLAY-EXIT.                      
    STOP RUN.                                                    
                                                                 
A-PARA.                                                          
    COMPUTE WS-TEMP = WS-I * 7 - 3                               
    MOVE 'A' TO WS-LETTER( 1, WS-TEMP )                          
    COMPUTE WS-TEMP = WS-I * 7 - 4                               
    MOVE 'A' TO WS-LETTER( 2, WS-TEMP )                          
    COMPUTE WS-TEMP = WS-I * 7 - 2                               
    MOVE 'A' TO WS-LETTER( 2, WS-TEMP )                          
    COMPUTE WS-TEMP = WS-I * 7 - 5                               
    MOVE 'A' TO WS-LETTER( 3, WS-TEMP )                          
    COMPUTE WS-TEMP = WS-I * 7 - 1                               
    MOVE 'A' TO WS-LETTER( 3, WS-TEMP )                          
    PERFORM VARYING WS-J FROM 4 BY 1 UNTIL WS-J > 7              
    COMPUTE WS-TEMP = WS-I * 7                                   
    MOVE 'A' TO WS-LETTER( WS-J, WS-TEMP )                       
    COMPUTE WS-TEMP = WS-I * 7 - 6                               
    MOVE 'A' TO WS-LETTER( WS-J, WS-TEMP )                       
    END-PERFORM                                                  
    PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 5              
    COMPUTE WS-TEMP = WS-I * 7 - WS-J                            
    MOVE 'A' TO WS-LETTER( 5, WS-TEMP )                          
    END-PERFORM.                                                 
A-EXIT. EXIT.                                                    
                                                                 
B-PARA.                                                          
    PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 6              
    COMPUTE WS-TEMP = WS-I * 7 - WS-J                            
    MOVE 'B' TO WS-LETTER( 1, WS-TEMP )                          
    COMPUTE WS-TEMP = WS-I * 7 - WS-J                            
    MOVE 'B' TO WS-LETTER( 7, WS-TEMP )                          
    END-PERFORM                                                  
    PERFORM VARYING WS-J FROM 2 BY 1 UNTIL WS-J > 6              
    COMPUTE WS-TEMP = WS-I * 7                                   
    MOVE 'B' TO WS-LETTER( WS-J, WS-TEMP )                       
    COMPUTE WS-TEMP = WS-I * 7 - 6                               
    MOVE 'B' TO WS-LETTER( WS-J, WS-TEMP )                       
    END-PERFORM                                                  
    PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 5              
    COMPUTE WS-TEMP = WS-I * 7 - WS-J                            
    MOVE 'B' TO WS-LETTER( 4, WS-TEMP )                          
    END-PERFORM.                                                 
B-EXIT. EXIT.                                                    
                                                                 
C-PARA.                                                          
    PERFORM VARYING WS-J FROM 2 BY 1 UNTIL WS-J > 6              
    COMPUTE WS-TEMP = WS-I * 7 - 6                               
    MOVE 'C' TO WS-LETTER( WS-J, WS-TEMP )                       
    END-PERFORM                                                  
    PERFORM VARYING WS-J FROM 0 BY 1 UNTIL WS-J = 6              
    COMPUTE WS-TEMP = WS-I * 7 - WS-J                            
    MOVE 'C' TO WS-LETTER( 1, WS-TEMP )                          
    MOVE 'C' TO WS-LETTER( 7, WS-TEMP )                          
    END-PERFORM.                                                 
C-EXIT. EXIT.                                                    
                                                                 
D-PARA.                                                          
    PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J = 7              
    COMPUTE WS-TEMP = WS-I * 7 - WS-J                            
    MOVE 'D' TO WS-LETTER( 1, WS-TEMP )                          
    MOVE 'D' TO WS-LETTER( 7, WS-TEMP )                          
    END-PERFORM                                                  
    PERFORM VARYING WS-J FROM 2 BY 1 UNTIL WS-J = 7              
    COMPUTE WS-TEMP = WS-I * 7                                   
    MOVE 'D' TO WS-LETTER( WS-J, WS-TEMP )                       
    COMPUTE WS-TEMP = WS-I * 7 - 6                               
    MOVE 'D' TO WS-LETTER( WS-J, WS-TEMP )                       
    END-PERFORM.                                                 
D-EXIT. EXIT.                                                    
                                                                 
E-PARA.                                                          
    PERFORM VARYING WS-J FROM 0 BY 1 UNTIL WS-J > 6              
    COMPUTE WS-TEMP = WS-I * 7 - WS-J                            
    MOVE 'E' TO WS-LETTER( 1, WS-TEMP )                          
    MOVE 'E' TO WS-LETTER( 7, WS-TEMP )                          
    END-PERFORM                                                  
    PERFORM VARYING WS-J FROM 2 BY 1 UNTIL WS-J = 7              
    COMPUTE WS-TEMP = WS-I * 7 - 6                               
    MOVE 'E' TO WS-LETTER( WS-J, WS-TEMP )                       
    COMPUTE WS-TEMP = WS-I * 7 - WS-J                            
    MOVE 'E' TO WS-LETTER( 4, WS-TEMP )                          
    END-PERFORM.                                                 
E-EXIT. EXIT.                                                    
                                                                 
F-PARA.                                                          
    PERFORM VARYING WS-J FROM 0 BY 1 UNTIL WS-J > 6              
    COMPUTE WS-TEMP = WS-I * 7 - WS-J                            
    MOVE 'F' TO WS-LETTER( 1, WS-TEMP )                          
    END-PERFORM                                                  
    PERFORM VARYING WS-J FROM 2 BY 1 UNTIL WS-J = 8              
    COMPUTE WS-TEMP = WS-I * 7 - 6                               
    MOVE 'F' TO WS-LETTER( WS-J, WS-TEMP)                        
    END-PERFORM                                                  
    PERFORM VARYING WS-J FROM 2 BY 1 UNTIL WS-J = 7              
    COMPUTE WS-TEMP = WS-I * 7 - WS-J                            
    MOVE 'F' TO WS-LETTER( 4, WS-TEMP )                          
    END-PERFORM.                                                 
F-EXIT. EXIT.                                                    
                                                                 
G-PARA.                                                          
    PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J = 6              
    COMPUTE WS-TEMP = WS-I * 7 - WS-J                            
    MOVE 'G' TO WS-LETTER( 1, WS-TEMP )                          
    MOVE 'G' TO WS-LETTER( 7, WS-TEMP )                          
    END-PERFORM                                                  
    PERFORM VARYING WS-J FROM 2 BY 1 UNTIL WS-J > 6              
    COMPUTE WS-TEMP = WS-I * 7 - 6                               
    MOVE 'G' TO WS-LETTER( WS-J, WS-TEMP )                       
    IF WS-J NOT EQUAL 3 THEN                                     
    COMPUTE WS-TEMP = WS-I * 7                                   
    MOVE 'G' TO WS-LETTER( WS-J, WS-TEMP )                       
    END-IF                                                       
    IF WS-J EQUAL 4 THEN                                         
    COMPUTE WS-TEMP = WS-I * 7 - 1                               
    MOVE 'G' TO WS-LETTER( WS-J, WS-TEMP )                       
    COMPUTE WS-TEMP = WS-I * 7 - 2                               
    MOVE 'G' TO WS-LETTER( WS-J, WS-TEMP )                       
    COMPUTE WS-TEMP = WS-I * 7 - 3                               
    MOVE 'G' TO WS-LETTER( WS-J, WS-TEMP )                       
    END-IF                                                       
    IF WS-J EQUAL 5 THEN                                         
    COMPUTE WS-TEMP = WS-I * 7 - 3                               
    MOVE 'G' TO WS-LETTER( WS-J, WS-TEMP )                       
    END-IF                                                       
    END-PERFORM.                                                 
G-EXIT. EXIT.                                                    
                                                                 
H-PARA.                                                          
    PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J = 8              
    COMPUTE WS-TEMP = WS-I * 7                                   
    MOVE 'H' TO WS-LETTER( WS-J, WS-TEMP )                       
    COMPUTE WS-TEMP = WS-I * 7 - 6                               
    MOVE 'H' TO WS-LETTER( WS-J, WS-TEMP )                       
    END-PERFORM                                                  
    PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J = 6              
    COMPUTE WS-TEMP = WS-I * 7 - WS-J                            
    MOVE 'H' TO WS-LETTER( 4, WS-TEMP )                          
    END-PERFORM.                                                 
H-EXIT. EXIT.                                                    
                                                                 
I-PARA.                                                          
    PERFORM VARYING WS-J FROM 0 BY 1 UNTIL WS-J > 6              
    COMPUTE WS-TEMP = WS-I * 7 - WS-J                            
    MOVE 'I' TO WS-LETTER( 1, WS-TEMP )                          
    MOVE 'I' TO WS-LETTER( 7, WS-TEMP )                          
    END-PERFORM                                                  
    PERFORM VARYING WS-J FROM 2 BY 1 UNTIL WS-J = 7              
    COMPUTE WS-TEMP = WS-I * 7 - 3                               
    MOVE 'I' TO WS-LETTER( WS-J, WS-TEMP )                       
    END-PERFORM.                                                 
I-EXIT. EXIT.                                                    
                                                                 
J-PARA.                                                          
    PERFORM VARYING WS-J FROM 0 BY 1 UNTIL WS-J > 6              
    COMPUTE WS-TEMP = WS-I * 7 - WS-J                            
    MOVE 'J' TO WS-LETTER( 1, WS-TEMP )                          
    END-PERFORM                                                  
    PERFORM VARYING WS-J FROM 2 BY 1 UNTIL WS-J = 8              
    IF WS-J EQUAL 5 OR WS-J EQUAL 6 THEN                         
    COMPUTE WS-TEMP = WS-I * 7 - 6                               
    MOVE 'J' TO WS-LETTER( WS-J, WS-TEMP )                       
    END-IF                                                       
    COMPUTE WS-TEMP = WS-I * 7 - 3                               
    MOVE 'J' TO WS-LETTER( WS-J, WS-TEMP )                       
    IF WS-J EQUAL 7 THEN                                         
    COMPUTE WS-TEMP = WS-I * 7 - 5                               
    MOVE 'J' TO WS-LETTER( WS-J, WS-TEMP )                       
    COMPUTE WS-TEMP = WS-I * 7 - 4                               
    MOVE 'J' TO WS-LETTER( WS-J, WS-TEMP )                       
    END-IF                                                       
    END-PERFORM.                                                 
J-EXIT. EXIT.                                                    
                                                                 
K-PARA.                                                          
    PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J = 8              
    IF WS-J EQUAL 1 OR WS-J EQUAL 7 THEN                         
    COMPUTE WS-TEMP = WS-I * 7                                   
    MOVE 'K' TO WS-LETTER( WS-J, WS-TEMP )                       
    COMPUTE WS-TEMP = WS-I * 7 - 6                               
    MOVE 'K' TO WS-LETTER( WS-J, WS-TEMP )                       
    END-IF                                                       
    IF WS-J EQUAL 2 OR WS-J EQUAL 6 THEN                         
    COMPUTE WS-TEMP = WS-I * 7 - 2                               
    MOVE 'K' TO WS-LETTER( WS-J, WS-TEMP )                       
    COMPUTE WS-TEMP = WS-I * 7 - 1                               
    MOVE 'K' TO WS-LETTER( WS-J, WS-TEMP )                       
    COMPUTE WS-TEMP = WS-I * 7 - 6                               
    MOVE 'K' TO WS-LETTER( WS-J, WS-TEMP )                       
    END-IF                                                       
    IF WS-J EQUAL 3 OR WS-J EQUAL 5 THEN                         
    COMPUTE WS-TEMP = WS-I * 7 - 4                               
    MOVE 'K' TO WS-LETTER( WS-J, WS-TEMP )                       
    COMPUTE WS-TEMP = WS-I * 7 - 3                               
    MOVE 'K' TO WS-LETTER( WS-J, WS-TEMP )                       
    COMPUTE WS-TEMP = WS-I * 7 - 6                               
    MOVE 'K' TO WS-LETTER( WS-J, WS-TEMP )                       
    END-IF                                                       
    IF WS-J EQUAL 4 THEN                                         
    COMPUTE WS-TEMP = WS-I * 7 - 6                               
    MOVE 'K' TO WS-LETTER( WS-J, WS-TEMP )                       
    COMPUTE WS-TEMP = WS-I * 7 - 5                               
    MOVE 'K' TO WS-LETTER( WS-J, WS-TEMP )                       
    END-IF                                                       
    END-PERFORM.                                                 
K-EXIT. EXIT.                                                    
                                                                 
L-PARA.                                                          
    PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J = 7              
    COMPUTE WS-TEMP = WS-I * 7 - 6                               
    MOVE 'L' TO WS-LETTER( WS-J, WS-TEMP )                       
    END-PERFORM                                                  
    PERFORM VARYING WS-J FROM 0 BY 1 UNTIL WS-J = 7              
    COMPUTE WS-TEMP = WS-I * 7 - WS-J                            
    MOVE 'L' TO WS-LETTER( 7, WS-TEMP )                          
    END-PERFORM.                                                 
L-EXIT. EXIT.                                                    
                                                                 
M-PARA.                                                          
    PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J = 8              
    COMPUTE WS-TEMP = WS-I * 7 - 6                               
    MOVE 'M' TO WS-LETTER( WS-J, WS-TEMP )                       
    COMPUTE WS-TEMP = WS-I * 7                                   
    MOVE 'M' TO WS-LETTER( WS-J, WS-TEMP )                       
    IF WS-J EQUAL 2 THEN                                         
    COMPUTE WS-TEMP = WS-I * 7 - 5                               
    MOVE 'M' TO WS-LETTER( WS-J, WS-TEMP )                       
    COMPUTE WS-TEMP = WS-I * 7 - 1                               
    MOVE 'M' TO WS-LETTER( WS-J, WS-TEMP )                       
    END-IF                                                       
    IF WS-J EQUAL 3 THEN                                         
    COMPUTE WS-TEMP = WS-I * 7 - 4                               
    MOVE 'M' TO WS-LETTER( WS-J, WS-TEMP )                       
    COMPUTE WS-TEMP = WS-I * 7 - 2                               
    MOVE 'M' TO WS-LETTER( WS-J, WS-TEMP )                       
    END-IF                                                       
    IF WS-J EQUAL 4 THEN                                         
    COMPUTE WS-TEMP = WS-I * 7 - 3                               
    MOVE 'M' TO WS-LETTER( WS-J, WS-TEMP )                       
    END-IF                                                       
    END-PERFORM.                                                 
M-EXIT. EXIT.                                                    
                                                                 
N-PARA.                                                          
    PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J = 8              
    COMPUTE WS-TEMP = WS-I * 7 - 6                               
    MOVE 'N' TO WS-LETTER( WS-J, WS-TEMP )                       
    COMPUTE WS-TEMP = WS-I * 7                                   
    MOVE 'N' TO WS-LETTER( WS-J, WS-TEMP )                       
    IF WS-J EQUAL 2 THEN                                         
    COMPUTE WS-TEMP = WS-I * 7 - 5                               
    MOVE 'N' TO WS-LETTER( WS-J, WS-TEMP )                       
    END-IF                                                       
    IF WS-J EQUAL 3 THEN                                         
    COMPUTE WS-TEMP = WS-I * 7 - 4                               
    MOVE 'N' TO WS-LETTER( WS-J, WS-TEMP )                       
    END-IF                                                       
    IF WS-J EQUAL 4 THEN                                         
    COMPUTE WS-TEMP = WS-I * 7 - 3                               
    MOVE 'N' TO WS-LETTER( WS-J, WS-TEMP )                       
    END-IF                                                       
    IF WS-J EQUAL 5 THEN                                         
    COMPUTE WS-TEMP = WS-I * 7 - 2                               
    MOVE 'N' TO WS-LETTER( WS-J, WS-TEMP )                       
    END-IF                                                       
    IF WS-J EQUAL 6 THEN                                         
    COMPUTE WS-TEMP = WS-I * 7 - 1                               
    MOVE 'N' TO WS-LETTER( WS-J, WS-TEMP )                       
    END-IF                                                       
    END-PERFORM.                                                 
N-EXIT. EXIT.                                                    
                                                                 
O-PARA.                                                          
    PERFORM VARYING WS-J FROM 2 BY 1 UNTIL WS-J = 7              
    COMPUTE WS-TEMP = WS-I * 7 - 6                               
    MOVE 'O' TO WS-LETTER( WS-J, WS-TEMP )                       
    COMPUTE WS-TEMP = WS-I * 7                                   
    MOVE 'O' TO WS-LETTER( WS-J, WS-TEMP )                       
    END-PERFORM                                                  
    PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J = 6              
    COMPUTE WS-TEMP = WS-I * 7 - WS-J                            
    MOVE 'O' TO WS-LETTER( 1, WS-TEMP )                          
    MOVE 'O' TO WS-LETTER( 7, WS-TEMP )                          
    END-PERFORM.                                                 
O-EXIT. EXIT.                                                    
                                                                 
P-PARA.                                                          
    PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J = 7              
    COMPUTE WS-TEMP = WS-I * 7 - WS-J                            
    MOVE 'P' TO WS-LETTER( 1, WS-TEMP )                          
    MOVE 'P' TO WS-LETTER( 4, WS-TEMP )                          
    END-PERFORM                                                  
    PERFORM VARYING WS-J FROM 2 BY 1 UNTIL WS-J = 4              
    COMPUTE WS-TEMP = WS-I * 7 - 6                               
    MOVE 'P' TO WS-LETTER( WS-J, WS-TEMP )                       
    COMPUTE WS-TEMP = WS-I * 7                                   
    MOVE 'P' TO WS-LETTER( WS-J, WS-TEMP )                       
    END-PERFORM                                                  
    PERFORM VARYING WS-J FROM 5 BY 1 UNTIL WS-J = 8              
    COMPUTE WS-TEMP = WS-I * 7 - 6                               
    MOVE 'P' TO WS-LETTER( WS-J, WS-TEMP )                       
    END-PERFORM.                                                 
P-EXIT. EXIT.                                                    
                                                                 
Q-PARA.                                                          
    PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J = 6              
    COMPUTE WS-TEMP = WS-I * 7 - WS-J                            
    MOVE 'Q' TO WS-LETTER( 1, WS-TEMP )                          
    MOVE 'Q' TO WS-LETTER( 6, WS-TEMP )                          
    END-PERFORM                                                  
    PERFORM VARYING WS-J FROM 2 BY 1 UNTIL WS-J = 6              
    COMPUTE WS-TEMP = WS-I * 7 - 6                               
    MOVE 'Q' TO WS-LETTER( WS-J, WS-TEMP )                       
    COMPUTE WS-TEMP = WS-I * 7                                   
    MOVE 'Q' TO WS-LETTER( WS-J, WS-TEMP )                       
    END-PERFORM                                                  
    COMPUTE WS-TEMP = WS-I * 7                                   
    MOVE 'Q' TO WS-LETTER( 7, WS-TEMP).                          
Q-EXIT. EXIT.                                                    
                                                                 
R-PARA.                                                          
    PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J = 7              
    COMPUTE WS-TEMP = WS-I * 7 - WS-J                            
    MOVE 'R' TO WS-LETTER( 1, WS-TEMP )                          
    MOVE 'R' TO WS-LETTER( 4, WS-TEMP )                          
    END-PERFORM                                                  
    PERFORM VARYING WS-J FROM 2 BY 1 UNTIL WS-J = 4              
    COMPUTE WS-TEMP = WS-I * 7 - 6                               
    MOVE 'R' TO WS-LETTER( WS-J, WS-TEMP )                       
    COMPUTE WS-TEMP = WS-I * 7                                   
    MOVE 'R' TO WS-LETTER( WS-J, WS-TEMP )                       
    END-PERFORM                                                  
    PERFORM VARYING WS-J FROM 5 BY 1 UNTIL WS-J = 8              
    COMPUTE WS-TEMP = WS-I * 7 - 6                               
    MOVE 'R' TO WS-LETTER( WS-J, WS-TEMP )                       
    COMPUTE WS-TEMP = WS-I * 7                                   
    MOVE 'R' TO WS-LETTER( WS-J, WS-TEMP )                       
    END-PERFORM.                                                 
R-EXIT. EXIT.                                                    
                                                                 
S-PARA.                                                          
    PERFORM VARYING WS-J FROM 0 BY 1 UNTIL WS-J = 6              
    COMPUTE WS-TEMP = WS-I * 7 - WS-J                            
    MOVE 'S' TO WS-LETTER( 1, WS-TEMP )                          
    END-PERFORM                                                  
    PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J = 7              
    COMPUTE WS-TEMP = WS-I * 7 - WS-J                            
    MOVE 'S' TO WS-LETTER( 7, WS-TEMP )                          
    END-PERFORM                                                  
    PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J = 6              
    COMPUTE WS-TEMP = WS-I * 7 - WS-J                            
    MOVE 'S' TO WS-LETTER( 4, WS-TEMP )                          
    END-PERFORM                                                  
    PERFORM VARYING WS-J FROM 2 BY 1 UNTIL WS-J = 4              
    COMPUTE WS-TEMP = WS-I * 7 - 6                               
    MOVE 'S' TO WS-LETTER( WS-J, WS-TEMP )                       
    END-PERFORM                                                  
    PERFORM VARYING WS-J FROM 5 BY 1 UNTIL WS-J = 7              
    COMPUTE WS-TEMP = WS-I * 7                                   
    MOVE 'S' TO WS-LETTER( WS-J, WS-TEMP )                       
    END-PERFORM.                                                 
S-EXIT. EXIT.                                                    
                                                                 
T-PARA.                                                          
    PERFORM VARYING WS-J FROM 0 BY 1 UNTIL WS-J = 7              
    COMPUTE WS-TEMP = WS-I * 7 - WS-J                            
    MOVE 'T' TO WS-LETTER( 1, WS-TEMP )                          
    END-PERFORM                                                  
    PERFORM VARYING WS-J FROM 2 BY 1 UNTIL WS-J = 8              
    COMPUTE WS-TEMP = WS-I * 7 - 3                               
    MOVE 'T' TO WS-LETTER( WS-J, WS-TEMP )                       
    END-PERFORM.                                                 
T-EXIT. EXIT.                                                    
                                                                 
U-PARA.                                                          
    PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J = 7              
    COMPUTE WS-TEMP = WS-I * 7 - 6                               
    MOVE 'U' TO WS-LETTER( WS-J, WS-TEMP )                       
    COMPUTE WS-TEMP = WS-I * 7                                   
    MOVE 'U' TO WS-LETTER( WS-J, WS-TEMP )                       
    END-PERFORM                                                  
    PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J = 6              
    COMPUTE WS-TEMP = WS-I * 7 - WS-J                            
    MOVE 'U' TO WS-LETTER( 7, WS-TEMP )                          
    END-PERFORM.                                                 
U-EXIT. EXIT.                                                    
                                                                 
V-PARA.                                                          
    PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J = 4              
    COMPUTE WS-TEMP = WS-I * 7 - 6                               
    MOVE 'V' TO WS-LETTER( WS-J, WS-TEMP )                       
    COMPUTE WS-TEMP = WS-I * 7                                   
    MOVE 'V' TO WS-LETTER( WS-J, WS-TEMP )                       
    END-PERFORM                                                  
    PERFORM VARYING WS-J FROM 4 BY 1 UNTIL WS-J = 6              
    COMPUTE WS-TEMP = WS-I * 7 - 5                               
    MOVE 'V' TO WS-LETTER( WS-J, WS-TEMP )                       
    COMPUTE WS-TEMP = WS-I * 7 - 1                               
    MOVE 'V' TO WS-LETTER( WS-J, WS-TEMP )                       
    END-PERFORM                                                  
    COMPUTE WS-TEMP = WS-I * 7 - 4                               
    MOVE 'V' TO WS-LETTER( 6, WS-TEMP )                          
    COMPUTE WS-TEMP = WS-I * 7 - 2                               
    MOVE 'V' TO WS-LETTER( 6, WS-TEMP )                          
    COMPUTE WS-TEMP = WS-I * 7 - 3                               
    MOVE 'V' TO WS-LETTER( 7, WS-TEMP ).                         
V-EXIT. EXIT.                                                    
                                                                 
W-PARA.                                                          
    PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J = 7              
    COMPUTE WS-TEMP = WS-I * 7 - 6                               
    MOVE 'W' TO WS-LETTER( WS-J, WS-TEMP )                       
    COMPUTE WS-TEMP = WS-I * 7                                   
    MOVE 'W' TO WS-LETTER( WS-J, WS-TEMP )                       
    IF WS-J EQUAL 5 THEN                                         
    COMPUTE WS-TEMP = WS-I * 7 - 3                               
    MOVE 'W' TO WS-LETTER( WS-J, WS-TEMP )                       
    END-IF                                                       
    IF WS-J EQUAL 6 THEN                                         
    COMPUTE WS-TEMP = WS-I * 7 - 4                               
    MOVE 'W' TO WS-LETTER( WS-J, WS-TEMP )                       
    COMPUTE WS-TEMP = WS-I * 7 - 2                               
    MOVE 'W' TO WS-LETTER( WS-J, WS-TEMP )                       
    END-IF                                                       
    END-PERFORM                                                  
    COMPUTE WS-TEMP = WS-I * 7 - 5                               
    MOVE 'W' TO WS-LETTER( 7, WS-TEMP )                          
    COMPUTE WS-TEMP = WS-I * 7 - 1                               
    MOVE 'W' TO WS-LETTER( 7, WS-TEMP ).                         
W-EXIT. EXIT.                                                    
                                                                 
X-PARA.                                                          
    MOVE 6 TO WS-L                                               
    PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J = 8              
    COMPUTE WS-TEMP = WS-I * 7 - WS-L                            
    MOVE 'X' TO WS-LETTER( WS-J, WS-TEMP )                       
    SUBTRACT 1 FROM WS-L                                         
    END-PERFORM                                                  
    MOVE 6 TO WS-L                                               
    PERFORM VARYING WS-J FROM 7 BY -1 UNTIL WS-J = 0             
    COMPUTE WS-TEMP = WS-I * 7 - WS-L                            
    MOVE 'X' TO WS-LETTER( WS-J, WS-TEMP )                       
    SUBTRACT 1 FROM WS-L                                         
    END-PERFORM.                                                 
X-EXIT. EXIT.                                                    
                                                                 
Y-PARA.                                                          
    MOVE 6 TO WS-L                                               
    PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J = 5              
    COMPUTE WS-TEMP = WS-I * 7 - WS-L                            
    MOVE 'Y' TO WS-LETTER( WS-J, WS-TEMP )                       
    SUBTRACT 1 FROM WS-L                                         
    END-PERFORM                                                  
    MOVE 6 TO WS-L                                               
    PERFORM VARYING WS-J FROM 7 BY -1 UNTIL WS-J = 0             
    COMPUTE WS-TEMP = WS-I * 7 - WS-L                            
    MOVE 'Y' TO WS-LETTER( WS-J, WS-TEMP )                       
    SUBTRACT 1 FROM WS-L                                         
    END-PERFORM.                                                 
Y-EXIT. EXIT.                                                    
                                                                 
Z-PARA.                                                          
    PERFORM VARYING WS-J FROM 0 BY 1 UNTIL WS-J = 7              
    COMPUTE WS-TEMP = WS-I * 7 - WS-J                            
    MOVE 'Z' TO WS-LETTER( 1, WS-TEMP )                          
    MOVE 'Z' TO WS-LETTER( 7, WS-TEMP )                          
    END-PERFORM                                                  
    MOVE 6 TO WS-L                                               
    PERFORM VARYING WS-J FROM 7 BY -1 UNTIL WS-J = 0             
    COMPUTE WS-TEMP = WS-I * 7 - WS-L                            
    MOVE 'Z' TO WS-LETTER( WS-J, WS-TEMP )                       
    SUBTRACT 1 FROM WS-L                                         
    END-PERFORM.                                                 
Z-EXIT. EXIT.                                                    
                                                                 
ASK-USER.                                                        
    DISPLAY 'ENTER A STRING. PLEASE LIMIT TO'                    
    DISPLAY 'MAX 10 CHARACTERS. THE PROGRAM '                    
    DISPLAY 'WILL NOT KNOW THE CHARACTERS   '                    
    DISPLAY 'ENTERED BEYOND 10 ;)           '                    
    ACCEPT WS-INPUT                                              
    DISPLAY ' '                                                  
    DISPLAY 'ENTERED STRING IS ' WS-INPUT                        
    DISPLAY ' '                                                  
    MOVE FUNCTION UPPER-CASE(WS-INPUT) TO WS-INPUT-UPPER
    INITIALIZE WS-PART1
               WS-PART2
    UNSTRING WS-INPUT DELIMITED BY SPACE INTO                    
             WS-PART1, WS-PART2                                  
    IF WS-PART2 NOT EQUAL SPACES THEN                            
       DISPLAY 'ENTER A STRING WITHOUT SPACES IN BETWEEN.'       
       PERFORM ASK-USER THRU ASK-EXIT                            
    ELSE                                                         
       INSPECT WS-INPUT TALLYING WS-COUNT FOR CHARACTERS.        
ASK-EXIT. EXIT.                                                  
                                                                 
DISPLAY-PARA.                                                    
    PERFORM ANOTHER-TABLE-PARA THRU ANOTHER-EXIT                 
            VARYING WS-K FROM 1 BY 1 UNTIL WS-K = 8              
    PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 7              
    DISPLAY WS-DISPLAY-LINE(WS-J)                                
    END-PERFORM                                                  
    DISPLAY ' '                                                  
    DISPLAY ' '.                                                 
DISPLAY-EXIT. EXIT.                                              
                                                                 
ANOTHER-TABLE-PARA.                                              
    MOVE 1 TO WS-L                                               
    PERFORM VARYING WS-J FROM 1 BY 7 UNTIL WS-J > 70             
    MOVE WS-LINE(WS-K)(WS-J:7) TO WS-DATA(WS-K, WS-L)            
    MOVE ' ' TO WS-FILLER(WS-K, WS-L)                            
    ADD 1 TO WS-L                                                
    END-PERFORM.                                                 
ANOTHER-EXIT. EXIT.        
