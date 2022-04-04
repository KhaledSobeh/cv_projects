CREATE TABLE dbo.Customers(
Customers_Email Varchar(50) NOT NULL,
Name Varchar(20) NOT NULL,
Gender char NULL,
Country Varchar(20) NOT NULL,
[Address-City] Varchar(20) NOT NULL,
[Address-Street] Varchar(20) NOT NULL,
[Address-Number] Varchar(20) NOT NULL,
Phone Varchar(20) NOT NULL,

CONSTRAINT PK_Customers PRIMARY KEY(Customers_Email),
CONSTRAINT ck_Customers_Email check (Customers_Email LIKE '%@%.%'),
CONSTRAINT ck_Phone check (Phone NOT LIKE '%[^0-9]%'),
CONSTRAINT ck_Gender check (Gender='M' or Gender='F'),
)

CREATE TABLE dbo.Members(
Customers_Email Varchar(50) NOT NULL,
Password Varchar(20) NOT NULL,

CONSTRAINT PK_Members Primary Key (Customers_Email),
CONSTRAINT FK_Member Foreign Key (Customers_Email) REFERENCES Customers(Customers_Email),
)

CREATE TABLE dbo.Searches(
IP Varchar(15) NOT NULL,
[Search-DT] DateTime NOT NULL,
Price Money NULL,
Size Varchar(20) NULL,
Runs Varchar(50) NULL,

CONSTRAINT PK_Searches Primary Key (IP,[Search-DT]),
CONSTRAINT ck_runs Foreign Key (Runs) REFERENCES Customers(Customers_Email),
)

CREATE TABLE dbo.Colors(
Color Varchar(30) Not Null primary key
)

CREATE TABLE dbo.[Search-Colors](
IP Varchar(15) NOT NULL,
[Search-DT] DateTime NOT NULL,
Color Varchar(30)  NOT NULL,

CONSTRAINT PK_Search_Colors Primary Key (IP,[Search-DT]),
CONSTRAINT FK_Search_Colors Foreign Key (IP,[Search-DT]) REFERENCES Searches(IP,[Search-DT]),
CONSTRAINT FK_Search_Color Foreign Key (Color) REFERENCES Colors(Color),
)

CREATE TABLE dbo. Styles (
Style Varchar(40) not null primary key
)

CREATE TABLE dbo.[Search-Styles](
IP Varchar(15) NOT NULL,
[Search-DT] DateTime NOT NULL,
Style  Varchar(40)  NULL,

CONSTRAINT PK_Search_Styles Primary Key (IP,[Search-DT]),
CONSTRAINT FK_Search_Styles Foreign Key (IP,[Search-DT]) REFERENCES Searches(IP,[Search-DT]),
CONSTRAINT FK_Search_Style Foreign Key (Style) REFERENCES Styles (Style),
)

CREATE TABLE dbo.Artists(
Artist_Email Varchar(50) NOT NULL,
Name Varchar(20) NOT NULL,
[Address-City] Varchar(20) NOT NULL,
[Address-Street] Varchar(20) NOT NULL,
[Address-Number] Varchar(20) NOT NULL,
Phone Varchar(20) NOT NULL,
DOB Date NOT NULL,
Gender Char  NULL,
Start_year Integer NOT NULL,
Background Varchar(200) NOT NULL,

CONSTRAINT PK_Artist PRIMARY KEY(Artist_Email),
CONSTRAINT ck_Artist_Email check (Artist_Email LIKE '%@%.%'),
CONSTRAINT ck_GenderA check (Gender='M' or Gender='F'),
)


CREATE TABLE dbo.Artworks(
Artist_Email Varchar(50) NOT NULL,
Name_artwork Varchar(20) NOT NULL,
Price Money  NOT NULL,
Size Varchar(20) NOT NULL,
ID_Order Varchar(20) Null,

CONSTRAINT PK_Artwork PRIMARY KEY(Artist_Email,Name_artwork),
CONSTRAINT FK_Artworks Foreign Key (Artist_Email) REFERENCES Artists(Artist_Email),
CONSTRAINT ck_Order Foreign Key (ID_Order) REFERENCES Orders(ID_Order),
)

CREATE TABLE dbo.[Artwork-Styles](
Artist_Email Varchar(50) NOT NULL,
Name_artwork Varchar(20) NOT NULL,
Style  Varchar(40)  NULL,

CONSTRAINT PK_Artwork_Styles Primary Key (Artist_Email, Name_artwork),
CONSTRAINT FK_Artwork_Styles Foreign Key (Artist_Email, Name_artwork) REFERENCES ArtWorks(Artist_Email, Name_artwork),
CONSTRAINT FK_Artwork_Style Foreign Key (Style) REFERENCES Styles (Style),
)

CREATE TABLE dbo.[Artwork-Colors](
Artist_Email Varchar(50) NOT NULL,
Name_artwork Varchar(20) NOT NULL,
Color  Varchar(30)  NULL,

CONSTRAINT PK_Artwork_Colors Primary Key (Artist_Email, Name_artwork),
CONSTRAINT FK_Artwork_Colors Foreign Key (Artist_Email, Name_artwork) REFERENCES ArtWorks(Artist_Email, Name_artwork),
CONSTRAINT FK_Artwork_Color Foreign Key (Color) REFERENCES Colors(Color),
)

CREATE TABLE dbo.Payment(
Number Real NOT NULL,
CVV Varchar (3) NOT NULL,
Name Varchar(20) NOT NULL,
Expiration_Date Char(5) NOT NULL,

CONSTRAINT PK_Payments Primary Key (Number),
CONSTRAINT ck_Number check (Number>=1000000000 AND Number<=9999999999),
CONSTRAINT ck_CVV check (len(CVV)=3),
CONSTRAINT ck_Exp_Date check (Expiration_Date LIKE '%%/%%'),

)

CREATE TABLE dbo.Orders(
ID_Order Varchar(20) NOT NULL,
Date_Order Date NOT NULL,
makes Varchar(50),
PayBy Real,

CONSTRAINT FK_Customer Foreign Key (makes) REFERENCES Customers (Customers_Email),
CONSTRAINT FK_Payment Foreign Key (PayBy) REFERENCES Payment(Number),
CONSTRAINT PK_Orders Primary Key (ID_Order),
)

CREATE TABLE dbo.Reviews(
Customers_Email Varchar(50)NOT NULL,
DT DateTime NOT NULL,
Rate Integer NULL,

CONSTRAINT PK_Reviews Primary Key (Customers_Email, DT),
CONSTRAINT FK_Customers_Reviews Foreign Key (Customers_Email) REFERENCES Customers(Customers_Email),
CONSTRAINT ck_Rate check ((len(Rate)=1) and ( (Rate) <=5 and (Rate) >=1))
)

CREATE TABLE dbo.WishList(
Customers_Email Varchar(50)NOT NULL,
Artist_Email Varchar(50) NOT NULL,
Name_artwork Varchar(20) NOT NULL,
Date_WishList Date NOT NULL,

CONSTRAINT PK_WishList Primary Key (Customers_Email ,Artist_Email, Name_artwork),
CONSTRAINT FK_Customers_WishList Foreign Key (Customers_Email) REFERENCES Customers(Customers_Email),
CONSTRAINT FK_Artist_WishList Foreign Key (Artist_Email) REFERENCES Artists(Artist_Email),
)

CREATE TABLE dbo.Finds (
IP Varchar(15) NOT NULL,
[Search-DT] DateTime NOT NULL,
Name VARCHAR(20) NOT NULL,
Email VARCHAR(50) NOT NULL,

CONSTRAINT PK_Finds  PRIMARY KEY (IP, [Search-DT], Name, Email),
CONSTRAINT FK_Finds Foreign Key (IP, [Search-DT]) REFERENCES Searches(IP, [Search-DT]),
CONSTRAINT FK_Finds1 Foreign Key (Email, Name) REFERENCES Artworks(Artist_Email,Name_artwork),
)

CREATE TABLE dbo.Gets(
Artist_Email VARCHAR(50) NOT NULL,
Customers_Email VARCHAR(50) NOT NULL,
DT dateTime NOT NULL,
Rate Integer Null,
CONSTRAINT PK_Gets  PRIMARY KEY (Artist_Email, Customers_Email, DT),
CONSTRAINT FK_Gets Foreign Key (Artist_Email) REFERENCES Artists(Artist_Email),
CONSTRAINT FK_Gets1 Foreign Key (Customers_Email, DT) REFERENCES Reviews(Customers_Email, DT),
CONSTRAINT ck_GetsRate check ((len(Rate)=1) and ( (Rate) <=5 and (Rate) >=1))
)