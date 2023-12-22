# Bereken de standaarddeviatie voor elke uitdaging
sd_vind_chatbot <- sd(ResultatenOIS$`Vinden Chatbot (s)`)
sd_retour_zonder_chatbot <- sd(ResultatenOIS$`Retourneren zonder chatbot (s)`)
sd_retour_met_chatbot <- sd(ResultatenOIS$`Retourneren met chatbot (s)`)
sd_voetbal_zonder_chatbot <- sd(ResultatenOIS$`Voetbal zonder chatbot`)
sd_voetbal_met_chatbot <- sd(ResultatenOIS$`Voetbal met chatbot`)
sd_cadeaubon_zonder_chatbot <- sd(ResultatenOIS$`Cadeaubon zonder chatbot`)
sd_cadeaubon_met_chatbot <- sd(ResultatenOIS$`Cadeaubon met chatbot`)
sd_bestellingen_zonder_chatbot <- sd(ResultatenOIS$`Bestellingen zonder chatbot`)
sd_bestellingen_met_chatbot <- sd(ResultatenOIS$`Bestellingen met chatbot`)
sd_wachtwoord_zonder_chatbot <- sd(ResultatenOIS$`wachtwoord zonder chatbot`)
sd_wachtwoord_met_chatbot <- sd(ResultatenOIS$`Wachtwoord met chatbot`)

# Toon de resultaten
cat("Standaarddeviatie Vind de chatbot:", sd_vind_chatbot, "\n")
cat("Standaarddeviatie Retourneren zonder chatbot:", sd_retour_zonder_chatbot, "\n")
cat("Standaarddeviatie Retourneren met chatbot:", sd_retour_met_chatbot, "\n")
cat("Standaarddeviatie Voetbal zonder chatbot:", sd_voetbal_zonder_chatbot, "\n")
cat("Standaarddeviatie Voetbal met chatbot:", sd_voetbal_met_chatbot, "\n")
cat("Standaarddeviatie Cadeaubon zonder chatbot:", sd_cadeaubon_zonder_chatbot, "\n")
cat("Standaarddeviatie Cadeaubon met chatbot:", sd_cadeaubon_met_chatbot, "\n")
cat("Standaarddeviatie Bestellingen zonder chatbot:", sd_bestellingen_zonder_chatbot, "\n")
cat("Standaarddeviatie Bestellingen met chatbot:", sd_bestellingen_met_chatbot, "\n")
cat("Standaarddeviatie Wachtwoord zonder chatbot:", sd_wachtwoord_zonder_chatbot, "\n")
cat("Standaarddeviatie Wachtwoord met chatbot:", sd_wachtwoord_met_chatbot, "\n")



# T-test voor Retourneren zonder chatbot VS Retourneren met chatbot
t_test_retour <- t.test(ResultatenOIS$`Retourneren zonder chatbot (s)`, ResultatenOIS$`Retourneren met chatbot (s)`)
cat("T-test voor Retourneren zonder chatbot VS Retourneren met chatbot:\n")
cat("p-waarde:", t_test_retour$p.value, "\n\n")

# T-test voor Voetbal in winkelmand zonder chatbot VS Voetbal in winkelmand met chatbot
t_test_voetbal <- t.test(ResultatenOIS$`Voetbal zonder chatbot`, ResultatenOIS$`Voetbal met chatbot`)
cat("T-test voor Voetbal zonder chatbot VS Voetbal met chatbot:\n")
cat("p-waarde:", t_test_voetbal$p.value, "\n\n")

# T-test voor Cadeaubon zonder chatbot VS Cadeaubon met chatbot
t_test_cadeaubon <- t.test(ResultatenOIS$`Cadeaubon zonder chatbot`, ResultatenOIS$`Cadeaubon met chatbot`)
cat("T-test voor Cadeaubon zonder chatbot VS Cadeaubon met chatbot:\n")
cat("p-waarde:", t_test_cadeaubon$p.value, "\n\n")

# T-test voor Bestellingen zonder chatbot VS Bestellingen met chatbot
t_test_bestellingen <- t.test(ResultatenOIS$`Bestellingen zonder chatbot`, ResultatenOIS$`Bestellingen met chatbot`)
cat("T-test voor Bestellingen zonder chatbot VS Bestellingen met chatbot:\n")
cat("p-waarde:", t_test_bestellingen$p.value, "\n\n")

# T-test voor Wachtwoord zonder chatbot VS Wachtwoord met chatbot
t_test_wachtwoord <- t.test(ResultatenOIS$`wachtwoord zonder chatbot`, ResultatenOIS$`Wachtwoord met chatbot`)
cat("T-test voor Wachtwoord zonder chatbot VS Wachtwoord met chatbot:\n")
cat("p-waarde:", t_test_wachtwoord$p.value, "\n\n")


# Vereiste bibliotheken laden
library(ggplot2)
library(dplyr)

# Dataframe voorbereiden met testnamen, p-waarden, en teststatistieken
data <- data.frame(
  Testnaam = c('Test1', 'Test2', ...),  # Vervang dit met je werkelijke testnamen
  P_waarde = c(p_waarde1, p_waarde2, ...),  # Vervang met je werkelijke p-waarden
  Teststatistiek = c(statistiek1, statistiek2, ...)  # Vervang met je werkelijke teststatistieken
)

# Grafiek maken
ggplot(data, aes(x = Testnaam)) +
  geom_bar(aes(y = P_waarde), stat = "identity", fill = "mediumseagreen") +
  geom_bar(aes(y = Teststatistiek / 10), stat = "identity", fill = "lightgreen", alpha = 0.6) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  labs(y = "P-waarde / (Teststatistiek / 10)", title = "Shapiro-Wilk P-waarden en Scores per Uitdaging") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Bibliotheek laden voor het maken van de grafiek
library(ggplot2)

# Data voorbereiden
categories <- c('Attraction', 'Transparency', 'Efficiency', 'Usability', 'Stimulation', 'Originality')
values <- c(0.8, 0.2, 0.3, 0.5, -0.3, -0.2)

# Dataframe maken
data <- data.frame(categories, values)

# Staafdiagram maken met ggplot2
ggplot(data, aes(x=categories, y=values, fill=categories)) +
  geom_bar(stat="identity") +
  ggtitle("UEQ for Chatbot") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=65, vjust=0.6),
        legend.position="none") +
  scale_fill_manual(values=c("blue"))

# Grafiek tonen
ggsave("UEQ_Chatbot.png", width = 10, height = 6, dpi = 300)


import matplotlib.pyplot as plt

# Uitdagingen en bijbehorende waarden (aangepast voor Nederlandse labels)
uitdagingen_nl = ["Voetbal", "Cadeaubon", "Bestellingen", "Wachtwoord wijzigen", "Pakket retourneren"]
gemiddelde_verschillen = [26, 8, -16, -11, -3]
p_waarden = [0.002, 0.008, 0.005, 0.002, 0.016]  # Aangepaste p-waarden

# De grafiek maken
fig, ax = plt.subplots(figsize=(8, 6))
bars = ax.bar(uitdagingen_nl, gemiddelde_verschillen, color=['#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd'])

# P-waarden als tekst boven/onder de staven
# Aanpassing van de positie dichter bij de staaf voor de voetbalstaaf.
for bar, p_value in zip(bars, p_waarden):
  y_val = bar.get_height()
vertical_alignment = 'bottom'
offset = 3 if y_val > 0 else -5  # Minder offset voor negatieve staven, dichter bij de staaf
ax.text(bar.get_x() + bar.get_width() / 2, y_val + offset, f'p = {p_value}',
        ha='center', va=vertical_alignment, fontsize=8, color='black')

# Lijn toevoegen bij y=0
ax.axhline(0, color='black', linewidth=0.8)

# Extra y-as waarde toevoegen voor -30
ax.set_ylim(-35, 40)

# Titel en labels toevoegen
plt.title('Gemiddelde resultaten van uitdagingen met p-waarden')
plt.xlabel('Uitdaging')
plt.ylabel('Gemiddeld verschil per taak (seconden)')
ax.set_xticklabels(uitdagingen_nl, rotation=45, ha='right')

# Onderliggende tekst toevoegen in het Nederlands
plt.figtext(0.1, -0.1, 'Gemiddeld verschil per taak (seconden). Positieve waarde: gebruik van chatbot is sneller', ha='left')

# Ruimte toevoegen voor tekst onderaan
plt.subplots_adjust(bottom=0.2)

# De grafiek tonen
plt.show()


categorieën <- c("Aantrekkelijkheid", "Transparantie", "Efficiëntie", "Bestuurbaarheid", "Stimulatie", "Originaliteit")
scores <- c(0.72, 0.71, 0.64, 0.62, 0.45, 0.10)

# Kleurenpalet met verschillende tinten blauw
kleurenpalet <- scales::brewer_pal(palette = "Blues")(length(categorieën))

# Maak een dataframe
data <- data.frame(Categorie = categorieën, AlphaScore = scores)

# Laad de bibliotheek ggplot2
library(ggplot2)

# Maak de grafiek met verschillende kleuren
grafiek <- ggplot(data, aes(x = reorder(Categorie, -AlphaScore), y = AlphaScore, fill = Categorie)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0.6, color = "red", linetype = "dashed") +
  labs(title = "Cronbach's Alpha Scores per Categorie UX",
       x = "Categorie UX vragen",
       y = "Cronbach's Alpha Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = kleurenpalet)

# Toon de grafiek
print(grafiek)


# Installeren en laden van benodigde pakketten
install.packages("ggplot2")
library(ggplot2)

# Definiëren van de categorieën en hun respectievelijke scores
categories <- c('Aantrekkelijkheid', 'Transparantie', 'Efficiëntie', 'Bestuurbaarheid', 'Stimulatie', 'Originaliteit')
scores <- c(0.047, 0.903, 0.315, 0.129, -0.397, -0.105)

# Omzetten van de categorieën en scores naar een dataframe
data <- data.frame(Categorie = categories, Score = scores)

# Aanmaken van de staafdiagram met ggplot
p <- ggplot(data, aes(x=Categorie, y=Score, fill=Categorie)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c('#1f77b4', '#2ca02c', '#ff7f0e', '#d62728', '#9467bd', '#8c564b')) +
  geom_hline(yintercept = 0, color='grey', linetype="dotted") +
  theme_minimal() +
  theme(axis.text.x = element_text(size=rel(0.8))) + # Kleiner lettertype voor de x-as labels
  labs(x='UX Categorieën', y='Gemiddelde UX Score per Categorie', 
       title='User Experience Vragenlijst Gemiddelde Resultaten per Categorie')

# Printen van de staafdiagram
print(p)
