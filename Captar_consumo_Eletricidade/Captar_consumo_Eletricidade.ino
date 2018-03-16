#include <SPI.h>
#include <Ethernet.h>
#include <EmonLib.h>


byte mac[] = { 0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED };
char server[] = "www.hasklupera-romefeller.c9users.io";
IPAddress ip(192, 168, 0, 177);
EthernetClient client;
EnergyMonitor emon1;

int rede = 220;
int pino_sct = A1;

void setup() {
  // Open serial communications and wait for port to open:
  Serial.begin(9600);
  while (!Serial) {
    ; // wait for serial port to connect. Needed for native USB port only
  }

  
  // disable SD SPI
  pinMode(4,OUTPUT);
  digitalWrite(4,HIGH);
  emon1.current(pino_sct, 60.6);
  // start the Ethernet connection:
  if (Ethernet.begin(mac) == 0) {
    Serial.println("Failed to configure Ethernet using DHCP");
    // try to congifure using IP address instead of DHCP:
    Ethernet.begin(mac, ip);
  }
  // give the Ethernet shield a second to initialize:
  delay(1000);
  Serial.println("connecting...");

  // if you get a connection, report back via serial:
 
}

void loop() {

  double Irms = emon1.calcIrms(1480);
  double integral;
  double fractional = modf(Irms, &integral);
  int direita = (int)(fractional*100);
  int esquerda = (int)integral;
  Serial.println("Potencia:  " + String(Irms));
  //Serial.println("Valor1Direita:    " +String(direita));
  //Serial.println("Valor2Esquerda:    " + String(esquerda));

if (client.connect(server, 80)) {
    
    String Linha1 = "GET /pag2/@left/@right/@pid/@aid HTTP/1.1";
    Linha1.replace("@left",String(esquerda));
    Linha1.replace("@right",String(direita));
    Linha1.replace("@pid","0");
    Linha1.replace("@aid","4");
    String Linha2 = "Host: https://hasklupera-romefeller.c9users.io";

    
    client.println(Linha1);
    client.println(Linha2);
    client.println();
    
    //Serial.println(Linha1 + "\r\n" + Linha2);
    delay(1000);
    client.stop();

    /*
    char test []="";
    String Consumo = "{\"qtConsumo\":23, \"dtConsumo\":\"2018-01-12\", \"ambienteId\":1, \"precoId\":0}";
    char Consumo2 [] = "{\"qtConsumo\":23.2, \"dtConsumo\":\"2018-01-12\", \"ambienteId\":1, \"precoId\":0}";
    String casa = "{\"nmCasa\":\"Bonita\"}";
    //String casa = "{nmCasa:\"Bonita\"}";

    String Linha1 = "POST https://hasklupera-romefeller.c9users.io/arduino/consumo HTTP/1.1";
    String Linha12 = "Accept-Encoding: gzip, deflate";
    String Linha2 = "User-Agent: Arduino";
    String Linha3 = "Host: https://hasklupera-romefeller.c9users.io";
    String Linha4 = "Content-Type: application/json";
    String Linha45 = "Accept: application/json";
    String Linha5 = "Content-Lenght: ";
    String Linha6 = String(Consumo.length());
    //String Linha7 = "'{\"nmCasa\":\"Bonita\"}'";
    String Linha7 = Consumo;
    String Bonus = "POST https://hasklupera-romefeller.c9users.io/arduino/consumo ";
    
    //String Com = Linha1 + "\n" + Linha2 + "\n" + Linha3 + "\n" + Linha4 + "\n" + Linha5 + Linha6 + "\n" + Linha7;
    //String Com = Linha1 + "\r\n" + Linha2 + "\r\n" + Linha3 + "\r\n" + Linha4 + "\r\n" + Linha5 + Linha6 + "\r\n\r\n" + Linha7;
    //String Com = Linha1 + "\r\n" + Linha3 + "\r\n\r\n";
    String Com = Linha1 + "\r\n"+ Linha11 + "\r\n" + Linha12 + "\r\n" + Linha4 + "\r\n" + Linha3 +"\r\n\r\n" + Linha7;
    Serial.println("connected");
    //client.println("POST /arduino/consumo HTTP/1.1");
    client.println("POST /casa HTTP/1.1");
    client.println("User-Agent: Arduino");
    client.println("Host: https://hasklupera-romefeller.c9users.io/");
    //client.print("Accept: *"); client.print("/"); client.println("*");
    //client.println("Connection: close");
    client.println("Content-Type: application/json");
    client.print("Content-Lenght: ");
    client.println(casa.length());
    client.print("\r\n");
    //client.println("Content-Type: application/x-www-form-urlencoded");
    //client.println(root.measureLength());
    //client.println(casa);
    client.println(casa);
    client.println();
    //client.println(casa);
    //root.printTo(client);
    //root.printTo(test);
    Serial.println(casa);
    Serial.println(Com);

    

    Serial.println("DEU BOM");
    
    client.println(Com);
    client.println();
    Serial.println(Com+"\n\n\n");

    delay(1000);
    client.stop();
 
    client.println("GET /consumoAmbiente/0 HTTP/1.1");
    client.println("Host: www.hasklupera-romefeller.c9users.io");
    
    client.println();
    Serial.println("DEU BOM");
    delay(1000);
    client.stop();
    */
    
  } else {
    // if you didn't get a connection to the server:
    Serial.println("connection failed");
 }
  
 
}

