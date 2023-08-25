# Introduction {#intro}

```{r, include=FALSE}
require(knitr)
require(tidyverse)
require(kableExtra)
require(huxtable)
require(TSP)
require(maps)
require(grid)

knitr::opts_chunk$set(echo = FALSE, results = "asis")

gameformat <- function(game, caption){
  gg <- as_hux(game) %>%
    set_width(ncol(game)/10) %>%
	  set_markdown() %>% 
    set_caption(caption) %>%
    set_bold(1, everywhere) %>%
    set_bold(everywhere, 1) %>%
    set_align(everywhere, everywhere, "center") %>%
#    set_right_border(everywhere, 1, 0.5) %>%
#    set_bottom_border(1, everywhere, 0.5) %>%
    set_right_border_color(everywhere, 1, "grey60") %>%
    set_bottom_border_color(1, everywhere, "grey60") %>%
    set_caption_pos("bottom") %>%
    set_row_height(everywhere, 0.6) %>%
  print_html(gg)
  # kable(game,
  #    format = 'latex',
  #  booktabs = T,
  #  escape = FALSE,
  #  align = paste0("r",strrep("c", ncol(game)-1)),
  #  linesep = "",
  #  caption = caption) %>%
  # column_spec(0:ncol(game), border_right = F) %>% 
  # column_spec(1,
  #            border_right = T,
  #            bold = T) %>%
  # row_spec(0, bold = T) %>%
  # sub("\\\\toprule", "", .) %>%
  # sub("\\\\bottomrule", "", .) %>%
  # sub("\\\\midrule", "\\\\hline", .)


# Have to add space between caption and table
# Do a sub command to replace \centring with a \vspace maybe?
# The forcing isn't working, and may not work for press anyway.
# Got to make references to each of the tables.
# Also something going weird with the chessboard; not sure what's up with that.
# Would be nice if I could just generate it
# It needs a fig.cap, and the other captions need periods.
# Maybe add row_spec(0, bold = T), and get rid of the $ in row names
}
```

## Two Questions {#twoquestions}

Decision theory should have at its heart two questions.

1.  What makes a decision good?
2.  How does one make good decisions?

It is of the first importance that these questions not be confused. The answers to the two questions are different, and the constraints on plausible answers to the two questions are different.

This book argues for a particular answer to the first question. I'll call this answer *causal ratificationism*. It says that what makes a decision good is that the decision maker can rationally defend it. Once the decision is made, the decision makes sense. The decision can be ratified by the decision maker. That's the reason for including 'ratificationism' in the name. The account I'll give of what it is to rationally ratify a decision gives a central role to causal concepts, which is why I've included the first word in the name.

Once I've set it out, causal ratificationism won't look too surprising to people familiar with contemporary game theory. But it does look fairly different to most contemporary decision theory. For example, it doesn't assign values to options, and instruct choosers to maximise value. Most contemporary work in decision theory does just that, with the primary disputes being over how to identify the values.

Causal ratificationism most closely resembles work in decision theory that follows @Skyrms1990 in looking at whether a chooser's views are in equilibrium. But most such views go on to identify a privileged equilibrium in decision problems that have multiple equilibria. Causal ratificationism does not. Very roughly, it just says that the chooser must rationally be in equilibrium, and then their choice is rational.

Causal ratificationism isn't just different from most existing theories. It violates all sorts of constraints that philosophical decision theorists have, implicitly or explicitly, imposed on theories of decision. I'll set out these constraints precisely in section \@ref(procdef), and call them collectively **proceduralism**. Much of the rest of the book will be a sustained argument against proceduralism. If one were trying to answer the second question I started with, how should one make good decisions, proceduralism would be a very plausible set of constraints. But they are not plausible constraints on answers to the first question, what makes a decision good.

If I'm going to rest so much argumentative weight on the difference between these questions, and on the different constraints on answers to them, I better say more about what the questions mean, and why I think they are so different. Let's turn to that first.

## Ideal Decision Theory

### Decisions Under Certainty {#deccertainty}

Most philosophical work on decision theory focusses on choices under uncertainty. And I'm going to follow suit for most of this book. But not yet. Instead, I'll start with an example of decision making under certainty, and in particular a version of the problem that Julia Robinson dubbed the 'travelling salesman' problem.[^demons-and-decisions-1]

[^demons-and-decisions-1]: The dubbing is in @Robinson1949. For a thorough history of the problem, see @Schrijver2005. For an accessible history of the problem, which includes these references, see the wikipedia page on 'Traveling Salesman Problem'.

> > **Salesman**\
> > Chooser is given the straight line distance between each pair of cities from the 257 represented on the map below. Using this information, Chooser has to find as short a path as possible that goes through all 257 cities and returns to the first one. The longer a path Chooser selects, the worse things will be for Chooser.

```{r salesman-points, fig.cap="The 257 cities that must be visited in the Salesman problem."}

theme_map <- function(base_size=9, base_family="") {
  theme_bw(base_size=base_size, base_family=base_family) %+replace%
    theme(axis.line=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid=element_blank(),
          panel.spacing=unit(0, "lines"),
          plot.background=element_blank(),
          legend.justification = c(0,0),
          legend.position = c(0,0)
    )
}

theme_set(theme_map())

all_states <- map_data("state") %>% 
  group_by(region) %>% 
  tally() %>% 
  select(state = region)

all_states$code <- c("AL", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA",
                     "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", 
                     "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", 
                     "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", 
                     "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

used_states <- 1:49

long_states <- all_states$state[used_states]
short_states <- all_states$code[used_states]

data("USCA312")
data("USCA312_GPS")

cities <- as_tibble(as.matrix(USCA312))

city_numbers <- tibble(
  id = 1:312,
  thecities = colnames(cities)
) %>% 
  mutate(used_city = case_when(str_sub(thecities, -2) %in% short_states  ~ 1,
                               TRUE ~ 0))

the_city_numbers <- filter(city_numbers, used_city == 1)$id


our_cities <- cities %>% 
  select(all_of(the_city_numbers)) %>% 
  slice(the_city_numbers)

our_gps <- USCA312_GPS %>% 
  slice(the_city_numbers) %>% 
  rowid_to_column()

city_matrix <- as.matrix(our_cities)

rownames(city_matrix) <- filter(city_numbers, used_city == 1)$thecities

## Fine tour
#tour_line <- solve_TSP(as.TSP(city_matrix), method="farthest_insertion")
#tour_line <- solve_TSP(as.TSP(city_matrix), method="two_opt", tour = tour_line)

## Not good tour
#tour_line <- solve_TSP(as.TSP(city_matrix), method="cheapest_insertion", start = 17) # - Very messy

## Generate tour by longitude - really bad
#tour_line <- TOUR(arrange(our_gps, long)$rowid, tsp = as.TSP(city_matrix))

## Best tour
load("tour_line.RData")

## Turn tour to map path
# paths <- tribble(
#   ~step, ~property, ~rowid, ~long, ~lat
# )
# 
# for (i in 1:nrow(our_gps)){
#   x <- tour_line[i]
#   first_city <- our_gps %>% slice(x)
#   next_city <- our_gps %>% slice(x %% 31)
#   paths <- paths %>% 
#     add_row(step = i, property = "from", rowid = first_city$rowid[1], long = first_city$long[1], lat = first_city$lat[1])# %>% 
#   #    add_row(step = i, property = "to", rowid = next_city$rowid[1], long = next_city$long[1], lat = next_city$lat[1])
# }
# 
# x <- tour_line[1]
# 
# paths <- paths %>% add_row(step = 24, property = "from", rowid = our_gps$rowid[x], long = our_gps$long[x], lat = our_gps$lat[x])


state_map_data <- map_data("state") %>%
  #  filter(subregion != "north" | is.na(subregion)) %>%
  filter(region %in% long_states) 

tour_map <- ggplot(state_map_data, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "grey90") + 
  geom_point(data = our_gps %>% select(long, lat), aes(x = long, y = lat), size = 0.25, inherit.aes = FALSE) +
#  geom_path(data = paths %>% select(long, lat), aes(x = long, y = lat), inherit.aes = FALSE, colour = "grey30", alpha = 0.5 ) + 
  coord_quickmap()
#tour_length(tour_line)
tour_map

tour_map_points_only <- tour_map

#str_c(our_gps$name, sep = "; ", collapse = "; ")
```

Since there are $256!$ possible paths, Chooser has a few options here.[^demons-and-decisions-2]

[^demons-and-decisions-2]: The 256 cities are the cities in the lower 48 states from the 312 cities in North America that John Burkardt mapped in his dataset Cities, available at [people.sc.fsu.edu/\~jburkardt/datasets/cities/cities.html](https://people.sc.fsu.edu/~jburkardt/datasets/cities/cities.html).

Question: What makes a particular choice of path a good one? Answer: It's no longer than the other $256!-1$ options.

Question: How should Chooser make a good choice here? Answer: Read a lot of the literature on this kind of optimisation problem, look for ways to rule out large collections of options all at once, etc.

These answers are quite different, and that means the questions are different to. The first question is asking something almost metaphysical - in virtue of what is the right answer the right one? The second question is practical - how do we go about finding that right answer, or at least getting close to it?

It would be really terrible to answer the second question by saying "choose the shortest route". This wouldn't be obviously incorrect. But it would be useless, which in context is almost as bad. The second question is asking a question in what we might call non-ideal decision theory.[^demons-and-decisions-3] And a constraint on a non-ideal theory is that it is useful, in a way that "choose the shortest route" is useless.

[^demons-and-decisions-3]: I'm borrowing the term 'non-ideal' from work in political philosophy. See @Valentini2012 for a good survey of some relevant work, and @Mills2005 for an important critique of the centrality of ideal theory in political philosophy. Critics of ideal theory, such as Mills, and @Sen2006, argue that we shouldn't base non-ideal theory on ideal theory. I'm going to agree, but my focus is primarily in the other direction. I'm going to argue that it isn't a constraint on ideal theory that it is useful in constructing a non-ideal theory.

But this is the right answer to the first question. The shortest route is, kind of by definition in this case, the best choice. That's the ideal. An ideal decision theory should say to choose it.[^demons-and-decisions-4]

[^demons-and-decisions-4]: The full list of cities in the Salesman problem is: `r str_c(our_gps$name, sep = "; ", collapse = "; ")`.

### Decisions Under Uncertainty {#decuncertain}

It's time to introduce uncertainty into the mix, because cases like Salesman might leave a misleading impression about what ideal decision theory tries to achieve.

> > **Basketball**\
> > Chooser is at a casino, and a basketball game is about to start. Chooser knows that basketball games don't have draws or ties; one side will win. And Chooser knows the teams are equally balanced; each team is 50% likely to win. Chooser has three options. They can bet on the Home team to win, bet on the Away team to win, or Pass, and not bet. If they bet, they win \$100 if the team they bet on wins, and lose \$110 if that team loses. If they Pass, they neither gain nor lose anything.

Ideal decision theory says that in Basketball, Chooser should Pass. That's not the optimal outcome for Chooser. The optimal outcome is that they bet on the winning team. But since they don't know who that is, and either bet will, on average, lose them money, they should Pass rather than bet on Home or Away.

Still, there is some sense in which they should not Pass - it doesn't produce the best outcomes. We could have a theory that just evaluated the possible outcomes in any decision. I'll call this Outcome Evaluation Theory, though it's more commonly called axiology. My name has the vice of being longer, but the virtue of using more familiar and descriptive language, so I'll stick with it.

Now there are three theories on the table: Outcome Evaluation Theory, Ideal Decision Theory, and Non-Ideal Decision Theory. One initial claim I want to make is that ordinary claims about what one should do are often three-way ambiguous, with the three ways corresponding to these three theories. Sometimes the claim is that one will produce the best outcome by doing this, sometimes that Ideal Decision Theory says to do it, and sometimes that Non-Ideal Decision Theory says to do it. Possibly the middle one, about Ideal Decision Theory, is the least commonly used in ordinary thought and talk, but it is a possible interpretation of a claim that one should do a particular thing. It's just that it's not the only possible interpretation of that claim.

We can distinguish these three theories by what they say to do in two examples introduced so far: Salesman and Basketball.

|     **Theory**     |    **Salesman**    | **Basketball** |
|:------------------:|:------------------:|:--------------:|
| Outcome Evaluation |   Shortest route   | Bet on winner  |
|   Ideal Decision   |   Shortest route   |      Pass      |
| Non-Ideal Decision | Study optimization |      Pass      |

: (#tab:three-theories) How three kinds of theories handle two problems.

Ideal Decision Theory is an odd mid-point between the two other theories listed. It takes Chooser's worldly knowledge as it is, and Chooser's mathematical abilities as they might be. That is, it idealises away from mathematical shortcomings, but not worldly ignorance. One question we might well ask is why we care about just these idealisations.

A related question is what benefit we gain from studying Ideal Decision Theory. Outcome Evaluation is obviously useful; we need to know what's better than what. Non-Ideal Decision Theory is obviously useful; it's good to make better decisions. But what do we gain from Ideal Decision Theory? The answer I'm going to give in chapter \@ref(defensive) is that Ideal Decision Theory is a vital input into worthwhile explanatory projects. But for now what I want to argue is that Ideal Decision Theory is distinct from Outcome Evaluation Theory and from Non-Ideal Decision Theory. And not just is it distinct, it's the project that philosophy decision theorists have primarily been engaged in for decades.

### Decision Theorists are Ideal {#areideal}

Most philosophical work in decision theory concerns what I've called Ideal Decision Theory. At least, that's what I'm going to argue in this subsection. And I'm going to argue for it with two caveats.

First, this particular usage of the Ideal/Non-Ideal distinction is mine. So it isn't that decision theorists are addressing Ideal Decision Theory under that label. But they are, I'll presently argue, implicitly addressing it.

Second, the reason I say 'most' not 'all' here is primarily to exclude what is called 'descriptive decision theory' [@ChandlerSEP] from the scope of the my claim. Descriptive decision theory concerns how people actually make decisions. Most of that work takes place outside philosophy, but I don't want to get into needless border wars to defend the claim I'm making. So I'll just note that philosophers working on descriptive decision theory are definitely not working on Ideal Decision Theory.

But Non-Ideal Decision Theory isn't Descriptive Decision Theory. It's a normative theory; it's about what people should do. The verdicts of Non-Ideal Decision Theory are that someone facing a problem like Salesman should learn some optimisation theory, that someone facing a novel and difficult situation should stop and have a think about their options, and so on. These are normative claims; they are about what the person should do. But they are not the kinds of claims that Outcome Evaluation Theory, or Ideal Decision Theory make. And my main sociological claim is that very few philosophers work on this normative but Non-Ideal branch of decision theory. Very few isn't zero, and there are philosophers who aim to take into account the kinds of limitations that Non-Ideal Decision Theory accounts for. But they are a small minority of decision theorists.

To see this, think about what most decision theorists say about our two examples: Salesman and Basketball. I'm not going to go over the range of what different decision theorists say case by case, for a couple of reasons. First, there are several good surveys of these views. The Stanford Encyclopedia alone has three surveys of different parts of the field [@SteeleSEP; @WeirichSEP; @BriggsSEP]. Second, and more importantly, they all say the same thing. They all say that Chooser should pick the shortest route in Salesman, and should Pass in Basketball.

These theories all say that Chooser should pick the shortest route in Salesman in part because as stated they assume perfect mathematical competence. But they do not assume perfect foresight, so they don't say Chooser should see into the future. It's not just that they assume perfect mathematical competence, and that's enough to solve Salesman. It's that the particular skills that one needs to solve Salesman are also needed to apply any of the theories of decision on the market.

Think about what it takes to solve Salesman by brute force. For each option, one has to look up 257 values, and sum them. Then one has to find the minimum value of the resulting sums.

Think about what it takes to maximise expected utility by brute force.[^demons-and-decisions-5] For each option, one has to calculate some values - products of utilities and probabilities - and sum the results of these calculations. Then one has to find the maximum value of the resulting sums.

[^demons-and-decisions-5]: I'll say more about what that means in a bit. For now, see @BriggsSEP for the details on expected utility.

These are more or less the same skills. Anyone who can find the maximum expected utility of any of an arbitrarily large set of options can find the shortest path from an arbitrarily large set of possible paths. Applying the most popular decision theory we have, expected utility theory, requires exactly the same skill as solving problems like Salesman. And we said that the signature difference between Ideal and Non-Ideal Decision Theory was that the former did, but the latter did not, abstract away from the fact that ordinary humans cannot simply solve these problems.

I'm not saying this to object to expected utility theory. Indeed, the theory I'm going to develop is just a version of expected utility theory. Rather, I'm saying it to support the claim that expected utility theory cannot be a theory of how to make good decisions. Instead, it's a theory of what makes a decision good. A chooser shouldn't calculate expected utilities for all their options. If they have, for example, \$256! options, they probably won't be able to. But a choice is good if it is the option with maximal expected utility. That's to say, expected utility theory is a form of Ideal Decision Theory.

And what goes for expected utility theory goes equally well for all theories out there. Treating any of them as a general theory of Non-Ideal Decision Theory would require that the person using them was able to solve problems at least as hard as the one in Salesman. And that's not something we should assume in a theory that's designed to be useful. Again, this isn't an objection to these theories. It's rather an argument that they should be interpreted as theories of Ideal Decision Theory. The only alternative is to interpret them in a way that Salesman is a quick refutation of each of them. That interpretation would be so uncharitable as to be wrong. Hence they are, as I said at the top of the subsection, attempts to offer an Ideal Decision Theory.

### The Keynesian Critique {#keynesian}

I'm going to spend some time in chapter \@ref(defensive) defending the study of Ideal Decision Theory. But we shouldn't give short shrift to the worry that maybe it shouldn't be. There are plenty of modern arguments to this effect, but for now I just want to briefly mention of the classics.

One of Keynes's most famous, most notorious, quotes concerns the relationship between Ideal and Non-Ideal theory in realms including Decision Theory. It's normally quoted wildly out of context, but here it is actually in context.

> > But this *long run* is a misleading guide to current affairs. *In the long run* we are all dead. Economists set themselves too easy, too useless a task if in tempestuous seasons they can only tell us that when the storm is long past the ocean will be flat again. [@Keynes1923, 80, emphasis in original]

Don't focus on the temporal connotations of Keynes's terminology of 'long run'. What's characteristic of his long run is not that it takes place in the distant future. What is characteristic of it instead is that it takes place in a world where some sources of interference are absent. It's a world where we sail but there are no storms. It's a study where we abstract away from storms and other unfortunate complications.

And that's what's characteristic of Ideal Decision Theory. We know that people cannot easily solve problems like the one in Salesman. But we think that, at least some of the time, it's worthwhile abstracting away from that particular human limitation. I think the connection between Ideal Decision Theory and the kind of equilibrium analysis that Keynes is critiquing here is even closer, since I think that notions of equilibria are at the heart of the right account of Ideal Decision Theory.

So let's say that Keynes's target here is excessive abstraction, and that there are many views that might fall within that target. Classical economics is the proximal target, but Ideal Decision Theory may be another, various views in political philosophy might be yet another, and so on. And Keynes makes, as I read him, two distinct criticisms of these kinds of views: they are too easy, and they are useless. Both of these seem like they are good criticisms of the view that says of Salesman that Chooser should simply choose the shortest route. Saying that is indeed both easy and useless.

In general though, the objection that these theories are 'easy' is not, I think, particularly fair. The most casual observation of twentieth century economics journals should make that clear. Even in the case Keynes uses, it isn't quite obvious why the sea should be flat once the storm passes. It won't be perfectly flat; the tides are not the result of any storm. But couldn't a big enough storm make semi-permanent shifts to the state of the ocean? If not, it takes some work to show this. A bit more seriously, by the time of the *General Theory*, Keynes himself thought economic storms could lead to equilibria with bad characteristics [@Keynes1936]. In economics, some storms do not leave flat seas in their wake.

On the other hand, the objection that we set ourselves too "useless" a task if we confine ourselves to Ideal Theory does ring true. Note, however, an important detail to this complaint. It's not that Ideal Theory is useless. It's that if we do nothing but Ideal Theory, if we only say that when the storm is over the seas are flat, we are not sufficiently useful. That all seems right, but it doesn't show that we should never do Ideal Theory. It just shows that we should never settle for just doing it.

Ideal Theory is an abstraction. In decision theory, it abstracts away from mathematical limitations. In economics, it might abstract away from various idiosyncratic features of individual economies. There is nothing wrong with abstraction as such. Without any abstraction, we just have a buzzing, blooming confusion of data. That's to say, for any problem, there is some abstraction that will be helpful in solving it. But there's a quantifier shift fallacy that's dangerously tempting around here. We might infer from every problem needs some abstraction, that there is some abstraction that is needed for every problem. And that latter claim is false. It's when you believe something like that that you end up settling for Ideal Theory, and set yourself, if not always too easy, then definitely too useless a task.

We should do more than Ideal Theory. But the rule there is collective. We, collectively, should do more than Ideal Theory. It doesn't follow that any one person, or any one book, must do more. And this book isn't, for the most part, going to do any more; this is a work in Ideal Theory. But I've belabored this point about the role of Ideal Theory in inquiry because the approach this book takes to Ideal Theory doesn't make sense if you see Ideal Theory as the pinnacle, or as the center, of inquiry. It's one task among many, and it has its limits. And the approach I'm going to take will make those limits clearer, and only make sense in light of those limits.

## Proceduralism and Ratificationism {#procdef}

The vast bulk of decision theories on the market are what I'll call proceduralist. The theory I'm going to offer, causal ratificationism, is not. Many people, I suspect, will see that as reason enough to reject causal ratificationism. But this would be a mistake. As I'll argue in this section, we should want our Non-Ideal Decision Theory to be proceduralist. But there is no obvious reason why an Ideal Decision Theory should be proceduralist.

### Proceduralism Defined {#procdefine}

A proceduralist decision theory provides a procedure for determining what to do in a given problem. The procedure might not be particularly efficient, or even remotely practical. I'll count things like *Add up the lengths in each path, then do a selection sort on the results*, as a procedure for solving Salesman, even if it would never complete in the available time. But let's abstract away from those details for now. Instead of focusing on computational complexity, I'll understand proceduralism about decision problems as the conjunction of four claims. Two of these are about the inputs to deliberation, and two of them are about the outputs. Here are the four claims, with some hopefully useful names.

Ex Ante

:   What is to be done is a function of what things are like at the start of deliberation.

Transparency

:   The inputs to that function are all knowable as long as the chooser is sufficiently rational and self-aware.

Decisiveness

:   In any decision problem, there is only one thing which is to be done, unless there are several things that are equally good. In the latter case, adding a minimal sweetening to any one of the equally good options would make it the option to be chosen.

Possibility

:   In any finite decision problem, at least one choice is rationally permissible. That is, there are no finite rational dilemmas.

I'm going to argue against all four of these claims. The kind of theory I favor, ratificationism, instead endorses the following four claims.

Ex Post

:   What is to be done is a function of what things are like at the end of deliberation.

Opacity

:   The inputs to that function involve, among other things, what probabilities are rational given the chooser's evidence, and this may be opaque to the self-aware, rational agent.

Indecisiveness

:   In many decision problems, there are permissible options which are not equally good, and there would still be many permissible options after sweetening one or other option.

Dilemmas

:   In many finite decision problems, no choice is rationally permissible. That is, there are finite rational dilemmas.

Neither proceduralism nor ratificationism is a package deal; you can mix and match the parts. And there are many natural weakenings of one or other part of the family of views. For instance, in chapter \@ref(decisive), I'll spend some time on views that say that Decisiveness is only guaranteed to hold in cases where there are just two options.

But that said, there are natural affinities between the four parts of proceduralism. if you thought the point of decision theory was to provide a user's guide to making decisions, you'll naturally end up with a proceduralist theory. And lots of theories have done just that. Any theory which starts with a function from states available to the chooser at the start of deliberation to numerical values, and instructs the chooser to maximise that value, will be proceduralist. That very abstract description of a decision theory covers the vast majority of theories on the market today.

Any theory of decision that assigns numerical values to each option on the basis of factors accessible to the chooser at the start of deliberation, and then exhorts the chooser to choose the option with the highest value, will be proceduralist. And if you're familiar with contemporary work in decision theory, you'll know that most theories on the market do indeed assign numerical values to each option on the basis of factors accessible to the chooser at the start of deliberation, and then exhort the chooser to choose the option with the highest value.

In a recent paper, Adam @Elga2021 describes a class of theories he calls 'suppositional', and notes that most existing theories of decision are suppositional. (And goes on to argue that we should want a theory to be suppositional.) The suppositional theories, as he describes them, are a subset of the procedural theories. Indeed, they are a subset of the theories described in the last paragraph - those that assign numerical values to choices. So saying that most theories on the market are proceduralist is not saying anything new. If anything, it's a commonplace.

**NOT COMPLETE**

## Causal Ratificationism {#cdintroduced}

It's going to take some setup to articulate precisely the positive theory I'm going to defend in this book. But I think it's worth having a rough statement of it up front, so you can see where we're headed.

According to causal ratificationism, a choice is rational if the following two conditions are met.

First, at the time the choice is made, no other choice has higher expected utility, given some probability distribution over the states that is rational at that time.

Second, at the time the choice is made, no other choice weakly dominates the choice.

There are a lot of technical terms there which I'll make clearer as this chapter goes along. But the key thing

**NOT COMPLETE**

# Make Ratifiable Decisions {#introchap}

A rational chooser knows what they are doing, and thinks that it is for the best. That is, they think that there is nothing else they could be doing that would be better. This book defends a version of decision theory that starts, and largely ends, with this principle. Properly understood, this is all there is to decision theory. But how to properly understand it will be the subject of much of this book.

The principle I just stated is backwards looking. It says that the chooser must think that the decision is for the best when they make it. It doesn't say much about how they come to make that decision, or whether the decision makes sense given the views the chooser has at the start of deliberation. That's by design. Decision theory is the theory of when decisions can be defended. Or, at least, that's what I'll argue in this book.

I'm mostly going to be concerned with a special class of decision problems: those involving demons who have spectacular predictive powers. These have been a particular focus of decision theorists for the last half-century. In keeping them center stage I am, in this one respect at least, following tradition. But I will make use of the principle that our theory of how to make decisions when demons are around should be consistent with our theory of how to make decisions when demons are not around. And the motivations for the parts of the theory that do and do not engage with demons should be consistent as well. This turns out to be a somewhat substantive constraint.

Demons predict what other people will choose, make moves accordingly, and these moves make a difference to the consequences of other choices. That's to say, demons behave just like the rational players in orthodox game theory. Interacting with demons is, at a fairly deep level, playing games with them. So we should expect game theory to have something to tell us about how those interactions go. This isn't a novel point; I owe it to William @Harper1986. But it is going to be central to the plot of this book.

The next three sections spell out the points made in each of the last three paragraphs. And then I'll close the chapter by setting out a generic version of the main example of the book, and going over the plans for the rest of the book.

## Basic Decision Theory {#bdt}

### Simple Decision Problems {#simpledecision}

A simple decision problem starts with a table like \@ref(tab:simple-table).

```{r, simple-table, cache=TRUE}
simple_table <- tribble(
	   ~"", ~L, ~R,
	   "U", "$v_{11}$", "$v_{12}$",
	   "D", "$v_{21}$", "$v_{22}$"
	)
gameformat(simple_table, "A generic two-by-two decision table.")
```

The rows we the options that the chooser, who I'll mostly call Chooser from now on, has. The columns list the possible states of the world. And in the cells set out the value to Chooser of each of these option-state pairs. Just to make the notation easier to remember, I've written $v_{ij}$ for the value of the outcome when Chooser selects option in row $i$, and the world is in state $j$.

Now there are a lot of questions one could have about that last paragraph, and I'll spend section \@ref(tables) going over five such questions at some length. But for now let's start with that basic picture. In general there are more than two possible options and more than two possible states, but let's start with this simple case for now and work up to the more complicated cases.

In order to make a decision, Chooser typically needs one more piece of information - how likely are the states $L$ and $R$? Let's add that information in: the probability of $L$ is $p_1$ and the probability of $R$ is $p_2$. What do we mean by 'probability' here? Good question, and one that I'll spend a lot of time on in chapter \@ref(coherence). For now, just use its colloquial meaning.

### Defining Basic Decision Theory {#bdtdefine}

Given all these facts, Chooser can assign a value to an option using the following formulae:

```{=tex}
\begin{align*}
V(U) &= p_1 v_{11} + p_2v_{12} \\
V(D) &= p_1 v_{21} + p_2v_{22} 
\end{align*}
```
And Chooser is rational iff they choose an option with maximal value.

In the more general case where $O_i$ is an arbitrary option, there are $k$ possible states, the probability of state $S_k$ is $p_k$, and the value of the combination of option $O_i$ and state is $S_k$, the value of option $O_i$ is given by this formula.

$$
V(O_i) = \sum_{j=1}^k p_j v_{ij}
$$

And in the even more general case where there are continuum many states, we need to use integrals to work out the value of each option. But these complexities aren't going to be particularly relevant to our story, so I'll return to the special two choice two option case, and note when the extra generalities are needed.

The formulae above are what are usually called the *expected* values of each option, and the decision theory I've just state is that if Chooser is rational, they maximise the expected value of their choice, given this probability distribution over the states of the world. Let's restate that not using variables, but explicitly using probabilities and values. Here $Pr$ is the probability function relevant to Chooser's decision, and $V$ is the value function. I'll use concatenation for conjunction, so $UL$ means that both $U$ and $L$ are true, i.e., that the first option is chosen and the first state is actualised. And let $O$ be an option, in this case a member of {U, D}.

$$
V(O) = Pr(L) V(OL) + Pr(R) V(OR)
$$

I'll call the theory that values each option this way, and says that rational choosers maximise value, Basic Decision Theory. It could just as easily be called the Crude Savage Decision Theory. The 'Savage' there is because the formula at the heart of it is the same formula that @Savage1954 puts at the heart of his decision theory. But the 'Crude' is there because Basic Decision Theory leaves off all that Savage says about the nature of options and states. @SteeleSEP [sect 3.1] have a good summary of what Savage says here. I'm not going to go into that. Instead I'll note why something more needs to be said. As it stands, Basic Decision Theory leads to absurd outcomes.

### Why Basic Decision Theory Fails {#notbasic}

Consider the St. Crispin's Day speech that Shakespeare has Henry V give before the Battle of Agincourt. (I'm indebted to a long ago conversation with Jamie Dreier for pointing out the connection between this speech and decision theory.) The background is that the English are about to go to battle with the French, and they are heavily outnumbered. Westmoreland wants to wait for more troops, and Henry does not, offering this reasoning.

> | What's he that wishes so?
> | My cousin Westmoreland? No, my fair cousin;
> | If we are marked to die, we are enough
> | To do our country loss; and if to live,
> | The fewer men, the greater share of honor.
> | God's will! I pray thee, wish not one man more.

It looks like Henry is suggesting that table \@ref(tab:agincourt) is the right model for their decision.

```{r, agincourt, cache=TRUE}
agincourt <- tribble(
	   ~"", ~`Victory`, ~`Defeat`,
	   "Attack", "$a$", "$c$",
	   "Wait", "$b$", "$d$"
	)
gameformat(agincourt, "Henry's reasoning on St. Crispin's Day.")
```

Henry argues, not implausibly, that $a > b$ since they will get more honour, and $c > d$, since they will lose fewer men. Now it doesn't matter what the probabilities of victory and defeat are. To see this, call them $x$ and $y$ respectively, and note that the two facts Henry mentions suffice to guarantee that $ax + cy > bx + dy$, assuming just that the probabilities are non-negative. So great, Henry is right, and they should attack. And they do, and they win, and all's well that end's well.

No! That's an absurd decision, even if it ended well on this occasion. Note that Henry's reasoning here is completely general - you could say the same before every battle. And it isn't true that you should always rush into battle with whoever you have on hand.

At one level, it's easy to say what has gone wrong here. There is too tight a connection between the choice Henry makes, and which state of the world is actual. But, and this is the philosophical problem, just what is the problematic connection between the choice and the state? This isn't obvious, because there are two different connections between the choice and the state, and it's a matter of some philosophical import which of them matters.

On the one hand, there is an evidential connection between the choice and the state. Learning that Henry has decided to go to battle rather than wait for reinforcements is evidence that he will lose. Misleading evidence, as it turned out, but certainly evidence. And maybe what's gone wrong in Henry's reasoning is that he has ignored that evidential connection in his reasoning.

On the other hand, there is a causal connection between the choice and the state. It's possible, given Henry's evidential situation at the time he makes the decision, not just that he is defeated, but that his rushing into battle causes his defeat. Relatedly, it's possible, for all Henry knows, that rushing into battle lowered the objective chance of his winning. Those last two sentences didn't quite say the same thing, and the differences between them will matter a little going forward, but for now we'll slide over their differences, and just note that there is in some natural sense a causal connection between Henry's decision and the resulting state of the battle.

So here we get to a point of common ground among contemporary decision theorists. It will, more or less, be the last point of common ground on the journey; from here everything gets contentious. When there is both an evidential and a causal connection between the possible choices and the possible states of the world, it is inappropriate to use Basic Decision Theory to make a decision. Indeed, in these cases, Basic Decision Theory will often validate the wrong decision.

It's not quite a universal view, and I'll talk a bit in appendix \@ref(quiggin) about people who don't believe it, but there is another very widely accepted claim in the vicinity of this one. When there is no evidential or causal connection between the possible choices and the possible states, most theorists think Basic Decision Theory recommends the right choice. Now they might not say, and typically do not say, that it makes the right recommendations for the right reasons. But they do say, at least most of them, that it gets the verdicts right. In the favored lingo of twentieth century philosophers, it is extensionally adequate in these cases.

So that covers the cases where there is both an evidential and causal connection - Basic Decision Theory gets things wrong - and the cases where there is neither - Basic Decision Theory gets things right. But what about the cases where there is one such connection but not the other? We're all taught that correlation is not causation. What happens when there is correlation but not causation between the choices and the states? Then things get really interesting, and that's the debate we're doing to jump into.

## Some Theories of Decision {#sometheories}

### Newcomb Problems {#newcombproblem}

It's not at all obvious how there could be a case where the possible choices and possible states could be causally connected but not evidentially connected. I'm going to set the possibility of such a case aside, at least until someone shows me what such a case might look like. Because there is a very natural way that the choices and states could be evidentially but not causally connected: they could have a common cause. And one way that could come about is if the states are predictions of Chooser's choice, made by someone who has a deep insight into Chooser's choice dispositions.

We'll call that someone Demon, and a decision problem in which the states are based in Demon's predictions a Demonic decision problem. I'll have much more to say about demons in section \@ref(aboutademon), but for now all we need to know is that Demon has the means, motive, and opportunity to correctly predict what strategy a Chooser will adopt.

The most famous Demonic decision problem is *Newcomb's Problem* [@Nozick1969]. Chooser is presented with two boxes, one opaque and one clear. They have a choice between taking just the opaque box, i.e., taking one box, and taking both the opaque and the clear box, i.e., taking two boxes. The clear box has a good of value $y$ in it. The contents of the opaque box are unknown. Demon has predicted the chooser's choice, and has placed a good of value $x$ in it if they predict Chooser will take one box, and left it empty (which we'll assume has value 0) if they predict Chooser will take both boxes. The key constraint is $x > y$. In most versions the value given for $x$ is massively greater than that for $y$, but the theories that are developed for the problem typically are sensitive only to whether $x$ is larger than $y$, not to how much larger it is.

Demon is really good at their job. They are not a time traveller; they are making a prediction that is not causally influenced by what the Chooser actually does. But they are really good. I'll assume that they are arbitrarily good, and come back to just what I mean by that in section \@ref(aboutademon).

I'll write 1 and 2 for the two choices, and P1 and P2 for the predictions. In general, where there is a demon who makes these kinds of predictions, I'll write 'PX' to mean the state of choice X being predicted. Table \@ref(tab:general-newcomb) sets out the problem.

```{r, general-newcomb, cache=TRUE}
general_newcomb <- tribble(
	   ~"", ~P1, ~P2,
	   "1", "$x$", "0",
	   "2", "$x + y$", "$y$"
	)
gameformat(general_newcomb, "Newcomb's Problem")
```

A large part of late twentieth century decision theory was given over to discussing this problem. So-called Causal Decision Theorists argued in favor of taking both boxes. The primary argument is that whatever the demon has done, the chooser gets a bonus of $y$ for taking the second box. It's good to get guaranteed bonuses, so they should take the bonus. This is basically the view I'm going to defend in this book, though with a number of deviations from the way it was defended in these classic works. So-called Evidential Decision Theorists argued in favor of taking just the one box. The primary argument is that the chooser who takes one box expects to get $x$, the chooser who expects to get both boxes expects to get $y$, it's better to take a choice that one expects to do better, and $x > y$, so it's better to take one box.

Both of these arguments trace back to the original presentation of the problem by Nozick. He named the problem after William Newcomb, a physicist from whom he learned of the problem. For much more detail on the background to this problem, and the motivation for the two prominent solutions, see @WeirichSEP. Let's turn to looking at those two solutions in more detail.

### Introducing EDT and CDT {#edtcdt}

The two most famous theories in recent work in decision theory are Causal Decision Theory and Evidential Decision Theory. I used these terms in subsection \@ref(newcombproblem) without defining them. It's time to do that now.

As I understand the way the terms are used, and indeed as I'll be using them, the terms are potentially misleading. Both of these are not really theories, but families of theories. Evidential Decision Theory (EDT) is a somewhat tighter family of theories than Causal Decision Theory (CDT), but neither is something that I would typically be happy calling a theory. In this section I'll give somewhat imprecise descriptions of each 'theory', starting with EDT. In section \@ref(edtcdtprecise) I'll say why both of these are really theory schema, and set out some of the more viable ways of making them into precise theories.

EDT, as I'm going to understand it, traces back to the first edition of Richard Jeffrey's *The Logic of Decision* [@Jeffrey1965]. The idea behind it is that what goes wrong with Henry's reasoning at Agincourt is that he ignores the fact that rushing into battle lowers the probability that he will win. In fact, according to EDT, the probability that he will win doesn't really matter to his decision. What matters is the probability that he will win if he attacks, and the probability that he will win if he waits for reinforcements. The value of each choice, according to EDT, is given by this formula

$$
V(O_i) = Pr(S_1 | O_i) V(S_1 O_i) + Pr(S_2 | O_i) V(S_2 O_i)
$$

And, as before, the one rule in decision theory is that one should maximise value.

So EDT says that Basic Decision Theory is incorrect. It uses probabilities of states, i.e., terms like $Pr(S_j)$, where it should use probabilities of states given choices, i.e., terms like $Pr(S_j | O_i)$. That's what goes wrong with Henry's reasoning.

In Newcomb's Problem, EDT says that one should take one box. Assume, for simplicity, that the probability that the demon will make correct predictions is 1. Then the value of taking one box is $x$, the value of taking two boxes is $y$, and by hypothesis $x >>> y$, so one should take one box.

CDT, or at least the version we're going to focus on for a while, traces back to David Lewis's paper *Causal Decision Theory* [@Lewis1981b]. Lewis actually has two aims in this paper: to set out a version of CDT, and to argue that the other versions don't differ in significant ways from his version. It's going to be somewhat important to the plotline of this book that Lewis's second claim, that the various versions of CDT don't greatly differ, is false. But the positive theory Lewis presents is interesting whether or not that second claim goes through, and that's what we'll focus on.

The idea is that Basic Decision Theory was not incorrect, as EDT says, but incomplete. It needs to be supplemented with rules about when the formula can be applied. In particular, we need to add that the states have to be causally independent of the options. In Lewis's terminology, the states have to be 'dependency hypotheses'. Each dependency hypothesis is something that the chooser has no causal influence over, and which determines, in conjunction with each possible act by the chooser, the probability of each possible outcome. If you apply the formula from Basic Decision Theory to cases where the states themselves depend (or even may depend) on the option, things go wrong. That's what CDT says goes wrong in Henry's case. It's the right formula, and he applies it correctly, but he shouldn't have started with simply *win* and *lose* as the states. Rather, he should have started with dependency hypotheses that do not causally depend upon his choices. For example, he could have started with the following three hypotheses: the troops we have now are enough to win; the troops we have now are not enough to win, but the troops we will have after reinforcements will be enough to win; and, even after getting reinforcements, we won't have enough troops to win. Since the middle of those states is very likely, and the utility of waiting for reinforcements is higher in that state, he probably should have waited.

In Newcomb's Problem, CDT says that one should take both boxes. What Demon predicts is not causally dependent on what Chooser selects. So we can use P1 and P2 as states. Let $z$ be the probability of P1, and hence the probability of P2 is $1-z$. Then the expected value of taking one box is $zx$, while the expected value of taking two boxes is $zx + y$. Without yet knowing what $z$ is, a question that will become rather important as we go on, we know that $zx + y > zx$, so taking two boxes has higher value. So that's what one should do.

### Making The Theories Precise {#edtcdtprecise}

So that's the basic picture of EDT and CDT. But as I alluded to earlier, setting out the basic picture isn't quite the same thing as setting out a theory. In this section I'll flag some factors that need to be settled to turn them into a theory.

-   What are probabilities?
-   Are they ex ante or ex post?

**NOT COMPLETE**

### Non-Unique Solutions {#nonunique}

Especially if using ex post

-   Say about 1; almost dominance
-   Say about 2; basically link to decisiveness chapter
-   Say about 0; basically link to dilemmas chapter

**NOT COMPLETE**

### Ratificationism {#ratificationismintroduced}

-   Just give a definition

**NOT COMPLETE**

## Games and Decisions {#gamesdecisions}

This section goes into a bit of detail about the connection between game theory and decision theory. If you want much more background on game theory, I've included some explanations of the key concepts in Appendix \@ref(gametheory). The point of this section is that the connection between game theory and decision theory is much tighter than a lot of theorists have realised.

### Newcomb Games {#newcombgames}

Let's start with an interesting variant of Newcomb's Puzzle, one which is used to good effect by Frank @Arntzenius2008. Keep the contents of the boxes the same, including that Demon puts $x$ into the first box iff they predict only one box will be taken. But this time both boxes are clear. Now Chooser can see exactly what is in each box before making a decision. What should they do?

We can model this problem using the tree in figure \@ref(fig:transparent-Newcomb).

```{tikz, transparent-Newcomb, fig.cap = "Newcomb's Problem with both boxes transparent", fit.ext = 'png', fig.width = 4, fig.height = 4, fig.align = 'center', cache=TRUE}
\begin{tikzpicture}[scale=1.5,font=\footnotesize]
\tikzset{
% Two node styles for game trees: solid and hollow
solid node/.style={circle,draw,inner sep=1.5,fill=black},
hollow node/.style={circle,draw,inner sep=1.5},
square node/.style={rectangle,draw,inner sep=1,fill=black}
}
% Specify spacing for each level of the tree
\tikzstyle{level 1}=[level distance=10mm,sibling distance=25mm]
\tikzstyle{level 2}=[level distance=10mm,sibling distance=15mm]
\tikzstyle{level 3}=[level distance=15mm,sibling distance=15mm]
\tikzstyle arrowstyle=[scale=1]
\tikzstyle directed=[postaction={decorate,decoration={markings,
mark=at position .5 with {\arrow[arrowstyle]{stealth}}}}]
% The Tree
\node(0)[hollow node,label=above:{$Demon$}]{}

child{node(2)[solid node, label=left:{$Chooser$}]{}
child{node[square node,label=below:{$x$}]{}edge from parent node[left]{1} }
child{node[square node,label=below:{$x+y$}]{} edge from parent node[right]{2}}
edge from parent node[left,yshift = 5]{P1}}
child{node(3)[solid node, label=right:{$Chooser$}]{}
child{node[square node,label=below:{$0$}]{}edge from parent node[left]{1} }
child{node[square node,label=below:{$y$}]{} edge from parent node[right]{2}}
edge from parent node[right,yshift = 5]{P2}
}
;
\end{tikzpicture}
```

This representation should look familiar from game theory textbooks. It's just a standard extensive form representation of a game where each player makes one move. Since we'll be using trees like this a bit, I want to explain the notation.

The game starts at the hollow node, which in this case is at the top of the tree. At each node, we move along a path from wherever we are to a subsequent node. So each node gets labeled with who is making the choice, and the edges get labeled with the choices they can make. This game starts with the Demon predicting either that Chooser takes 1 box - this is the edge labeled P1 - or that Chooser takes 2 boxes. Either way we get to a node where Chooser moves, either by taking 1 box or 2. It's a solid node, which means (in the notation of this book) that it's not where the game starts, and it's not where the game ends. Then whatever happens, we get to a terminal node, here denoted with a square. At each terminal node we list the payouts.

But here we only listed the payouts to Chooser. To make something really into a game, there should be payouts for both players. What are Demon's payouts? Well, what makes something the payout function for a player is that it takes higher values the more they get what they want. Since Demon is trying to predict player, they want situations where they predict them well. So we can simply say that their payout is 1 for a correct prediction, and 0 for an incorrect prediction. That suggests the tree for Arntzenius's 'transparent box' version of Newcomb's Problem should look like figure \@ref(fig:transparent-Newcomb-two-payouts).

```{tikz, transparent-Newcomb-two-payouts, fig.cap = "Newcomb's Problem with both boxes transparent, and Demon's payouts listed", fit.ext = 'png', fig.width = 4, fig.height = 4, fig.align = 'center', cache=TRUE}
\begin{tikzpicture}[scale=1.5,font=\footnotesize]
\tikzset{
% Two node styles for game trees: solid and hollow
solid node/.style={circle,draw,inner sep=1.5,fill=black},
hollow node/.style={circle,draw,inner sep=1.5},
square node/.style={rectangle,draw,inner sep=1,fill=black}
}
% Specify spacing for each level of the tree
\tikzstyle{level 1}=[level distance=10mm,sibling distance=25mm]
\tikzstyle{level 2}=[level distance=10mm,sibling distance=15mm]
\tikzstyle{level 3}=[level distance=15mm,sibling distance=15mm]
\tikzstyle arrowstyle=[scale=1]
\tikzstyle directed=[postaction={decorate,decoration={markings,
mark=at position .5 with {\arrow[arrowstyle]{stealth}}}}]
% The Tree
\node(0)[hollow node,label=above:{$Demon$}]{}

child{node(2)[solid node, label=left:{$Chooser$}]{}
child{node[square node,label=below:{$x,1$}]{}edge from parent node[left]{1} }
child{node[square node,label=below:{$x+y,0$}]{} edge from parent node[right]{2}}
edge from parent node[left,yshift = 5]{P1}}
child{node(3)[solid node, label=right:{$Chooser$}]{}
child{node[square node,label=below:{$0,0$}]{}edge from parent node[left]{1} }
child{node[square node,label=below:{$y,1$}]{} edge from parent node[right]{2}}
edge from parent node[right,yshift = 5]{P2}
}
;
\end{tikzpicture}
```

I've put Demon's payouts second, even though Demon moves first. The focus here is on Chooser, so they are player 1. When a game representation lists the payout in a situation as $a, b$ that means that player 1 gets $a$ and player 2 gets $b$. In this case that means the chooser gets $a$ and the demon gets $b$.

In this book I'm mostly going to work with games where Demon's payouts are either 1 for a correct prediction of 0 for an incorrect one. But once we've got the basic concept of Demon as a player getting payouts, we can set the demon up with other payouts too. And then we can bring just about any tool we like from contemporary game theory to bear on demonic decision theory.

That move, of treating Newcomb Problems as games, is taken straight from work by William @Harper1986. And it is going to be the central move in this book.

When we follow Harper's lead and transform the original Newcomb Problem into a game, we get table \@ref(tab:general-newcomb-two).

```{r general-newcomb-two, cache=FALSE}
general_newcomb <- tribble(
	   ~"", ~P1, ~P2,
	   "1", "$x, 1$", "$0, 0$",
	   "2", "$x + y, 0$", "$y, 1$"
	)
gameformat(general_newcomb, "Newcomb's Problem as a Game")
```

Or, at least, that's the so-called strategic form of the game. We can also represent it, as in figure \@ref(fig:hidden-Newcomb-two-payouts) as a game that takes place over time.

```{tikz, hidden-Newcomb-two-payouts, fig.cap = "Newcomb's Problem with first box hidden, and Demon's payouts listed", fit.ext = 'png', fig.width = 4, fig.height = 4, fig.align = 'center', cache=TRUE}
\usetikzlibrary{calc} 
\begin{tikzpicture}[scale=1.5,font=\footnotesize]
\tikzset{
% Two node styles for game trees: solid and hollow
solid node/.style={circle,draw,inner sep=1.5,fill=black},
hollow node/.style={circle,draw,inner sep=1.5},
square node/.style={rectangle,draw,inner sep=1,fill=black}
}
% Specify spacing for each level of the tree
\tikzstyle{level 1}=[level distance=10mm,sibling distance=25mm]
\tikzstyle{level 2}=[level distance=10mm,sibling distance=15mm]
\tikzstyle{level 3}=[level distance=15mm,sibling distance=15mm]
\tikzstyle arrowstyle=[scale=1]
\tikzstyle directed=[postaction={decorate,decoration={markings,
mark=at position .5 with {\arrow[arrowstyle]{stealth}}}}]
% The Tree
\node(0)[hollow node,label=above:{$Demon$}]{}

child{node(2)[solid node]{}
child{node[square node,label=below:{$x,1$}]{}edge from parent node[left]{1} }
child{node[square node,label=below:{$x+y,0$}]{} edge from parent node[right]{2}}
edge from parent node[left,yshift = 5]{P1}}
child{node(3)[solid node]{}
child{node[square node,label=below:{$0,0$}]{}edge from parent node[left]{1} }
child{node[square node,label=below:{$y,1$}]{} edge from parent node[right]{2}}
edge from parent node[right,yshift = 5]{P2}
}
;
\draw[dashed](2) to (3);
\node at ($(2)!.5!(3)$)[above]{$Chooser$};
\end{tikzpicture}
```

The dashed line there represents that those two nodes are in what game theorists call an **information set**. That means that when the player to move reaches one of those nodes, all they know is that they are at one of these nodes and not any other. In this case, Chooser knows that they have to select 1 box or 2, and they know the payouts given their choice and Demon's prediction. But they do not know what Demon predicted, so they do not know which node they are at.

This extensive form representation is in a way more accurate than the strategic form representation in the table above. It encodes that Demon goes first, which is something usually stressed in the story that is told about Newcomb's Problem. But the table form is easier to read, and makes clearer that there is only one equilibrium of the game: Demon makes prediction P2 and Chooser chooses 2. So I'll mostly use tables when they are possible. And they often are possible - lots of games can be turned into demonic decision problems like Newcomb's Problem.

### Familiar Games {#familiar}

Much of what happens in this book comes from seeing demonic decision problems as games and, conversely, seeing games as potential demonic decision problems. So I want to spend a little time setting out how the translation between the two works.

Transforming a demonic decision problem into a game is easy. As I noted, you just replace the states generated by Demon's choices with moves for Demon, and give them payout 1 if they predict correctly, and 0 otherwise.

You might worry that this only gives you cases where Demon is approximately perfect, but we also want cases where the demon is, say, 80% accurate. But that's easy to do as well. In fact there are two ways to do it.

The first is what I'll call the Selten strategy, because it gives the demon a 'trembling hand' in the sense of @Selten1975. Instead of letting Demon choose a state in the original problem, let Demon choose one of $n$ buttons, where $n$ is the number of choices the (human) chooser has. Each button is connected to a probabilistic device that generates one of the original states. If you want Demon to be 80% accurate, say the button $b_i$ associated with option $o_i$ outputs state $s_i$ with probability 0.8, and each of the other states with probability $\frac{0.2}{n - 1}$. And still say that Demon gets payout 1 for any $i$ if the chooser selects $o_i$ and the button generates state $s_i$, and 0 otherwise.

The second is what I'll call the Smullyan strategy, because it involves a Knights and Knaves puzzle of the kind that play a role in several of Smullyan's books, especially his [-@Smullyan1978]. Here the randomisation takes place before Demon's choice. Demon is assigned a type Knight or Knave. Demon is told of the assignment, but Chooser is not. If Demon is assigned type Knight, the payouts stay the same as in the game where Demon is arbitrarily accurate. If Demon is assigned type Knave, the payouts are reversed, and Demon gets payout 1 for an incorrect prediction.

There are benefits to each approach, and there are slightly different edge cases that are handled better by one or other version. I'm mostly going to stick to cases where Demon is arbitrarily accurate, but I need these on the table to talk about cases others raise where Demon is only 75-80% accurate. And in general either will work for turning a demonic decision problem into a game.

Turning games into demonic decision problems is a bit more interesting. Start with a completely generic two-player, two-option, simultaneous move, symmetric game, as shown in table \@ref(tab:basic-sym-game). We won't only look at symmetric games, but it's a nice way to start.

```{r,basic-sym-game, cache=TRUE}
symmetric_game <- tribble(
	   ~"", ~A, ~B,
	   "A", "$x, x$", "$y, z$",
	   "B", "$z, y$", "$w, w$"
	)
gameformat(symmetric_game, "A generic symmetric game.")
```

In words, what this says is that each player chooses either A or B. If they both choose A, they both get $x$. If they both choose B, they both get $w$. And if one chooses A and the other chooses B, the one who chooses A gets $y$ and the one who chooses B gets $z$. (Note that the payouts list row's payment first, if you're struggling to translate between the table and the text.) A lot of famous games can be defined in terms of restrictions on the four payout values. For example, a game like this is a Prisoners' Dilemma if the following constraints are met.

-   $x > z$
-   $y > w$
-   $w > x$

Some books will also add $2x > y + z$ as a further constraint, but I'll stick with these three.

Now to turn a game into a demonic decision problem, first replace column's payouts with 1s and 0s, with 1s along the main diagonal, and 0s everywhere else. Table \@ref(tab:demon-sym-game) shows what a generic symmetric game looks like after this transformation.

```{r,demon-sym-game,cache=TRUE}
demonic_symmetric_game <- tribble(
	   ~"", ~A, ~B,
	   "A", "$x, 1$", "$y, 0$",
	   "B", "$z, 0$", "$w, 1$"
	)
gameformat(demonic_symmetric_game, "The demonic version of a generic symmetric game.")
```

The next step is to replace Demon's moves with states that are generated by Demon's predictions. As before, I'll put 'P' in front of a choice name to indicate the state of that choice being predicted. The result is table \@ref(tab:gen-dem-problem).

```{r,gen-dem-problem, cache=TRUE}
generic_demonic_problem <- tribble(
	   ~"", ~PA, ~PB,
	   "A", "$x$", "$y$",
	   "B", "$z$", "$w$"
	)
gameformat(generic_demonic_problem, "The demonic decision problem generated by a generic symmetric game.")
```

If we add the constraints $x > z, y > w, w > x$, this is essentially a Newcomb Problem. I'm a long way from the first to point out the connections between Prisoners' Dilemma and Newcomb's Problem; it's literally in the title of a David Lewis paper [@Lewis1979e]. But what I want to stress here is the recipe for turning a familiar game into a demonic problem.

We can do the same thing with Chicken. The toy story behind Chicken is that two cars are facing off at the end of a road. They will drive straight at each other, and at the last second, each driver will choose to swerve off the road, which we'll call option A, or stay on the road, which we'll call option B. If one swerves and the other stays, the one who stays is the winner. If they both swerve they both lose and it's boring, and if they both stay it's a fiery crash. So in terms of the payouts in the general symmetric game, the constraints are:

-   $x < z$
-   $y >> w$
-   $x >> w$

Just what it means for one value to be much more than another, which is what I mean by '$>>$', is obviously vague. Table \@ref(tab:basic-chicken) gives an example with some numbers that should satisfy it.

```{r, basic-chicken, cache=TRUE}
basic_chicken <- tribble(
	   ~"", ~A, ~B,
	   "A", "$0, 0$", "$0, 1$",
	   "B", "$1, 0$", "$-100, -100$"
	)
gameformat(basic_chicken, "A version of Chicken.")
```

Replace the other driver, the one who plays column in this version, with a Demon, who only wants to predict row's move. The result is table \@ref(tab:demon-chicken).

```{r,demon-chicken, cache=TRUE}
demon_chicken <- tribble(
	   ~"", ~A, ~B,
	   "A", "$0, 1$", "$0, 0$",
	   "B", "$1, 0$", "$-100, 1$"
	)
gameformat(demon_chicken, "A demonic version of Chicken.")
```

All I've done to generate table \@ref(tab:demon-chicken) is replace column's payouts with 1s on the main diagonal, and 0s elsewhere. The next step is to replace the demonic player with states generated by Demon's choices. The result is table \@ref(tab:egan-game).

```{r,egan-game, cache=TRUE}
egan_game <- tribble(
	   ~"", ~PA, ~PB,
	   "A", "$0$", "$0$",
	   "B", "$1$", "$-100$"
	)
gameformat(egan_game, "A demonic decision problem based on Chicken.")
```

And table \@ref(tab:egan-game) is just the Psychopath Button example that Andy @Egan2007 raises as a problem for Causal Decision Theory.

Another familiar game from introductory game theory textbooks is matching pennies. This is a somewhat simplified version of rock-paper-scissors. Each player has a penny, and they reveal their penny simultaneously. They can either show it with the heads side up (option A), or the tails side up (option B). We specify in advance who wins if they show the same way, and who wins if they show opposite ways. So let's say column will win if both coins are heads or both are tails, and row will win if they are different. The payouts are shown in table \@ref(tab:match-pennies).

```{r,match-pennies, cache=TRUE}
matching_pennies <- tribble(
	   ~"", ~A, ~B,
	   "A", "$0, 1$", "$1, 0$",
	   "B", "$1, 0$", "$0, 1$"
	)
gameformat(matching_pennies, "The game matching pennies.")
```

This isn't a symmetric game, but it is already demonic. Column's payouts are 1 in the main diagonal and 0 elsewhere. So we can convert it to a demonic decision problem fairly easily, as in table \@ref(tab:death-in-damascus).

```{r,death-in-damascus, cache=TRUE}
d_i_d <- tribble(
	   ~"", ~PA, ~PB,
	   "A", "$0$", "$1$",
	   "B", "$1$", "$0$"
	)
gameformat(d_i_d, "Matching pennies as a decision problem")
```

And table \@ref(tab:death-in-damascus) is the familiar problem Death in Damascus from @GibbardHarper1978.

Let's do one last one, starting with the familiar game Battle of the Sexes. Row and Column each have to choose whether to do R or C. They both prefer doing the same thing to doing different things. But Row would prefer they both do R, and Column would prefer they both do C. (The original name comes from a version of the story where Row and Column are a heterosexual married couple, and Row wants to do some stereotypically male thing, while Column wants to do some stereotypically female thing. That framing is tiresome at best, but the category of asymmetric coordination games is not, hence my more abstract presentation.) So table \@ref(tab:bach-stravinsky) is one way we might think of the payouts.

```{r,bach-stravinsky, cache=TRUE}
b_o_t_s <- tribble(
	   ~"", ~R, ~C,
	   "R", "$4, 1$", "$0, 0$",
	   "C", "$0, 0$", "$1, 4$"
	)
gameformat(b_o_t_s, "A version of battle of the sexes.")
```

As it stands, that's not a symmetric game. But we can make it a symmetric game by relabeling the choices. Let option A for each player be doing their favored choice, and option B be doing their less favored choice. That turns table \@ref(tab:bach-stravinsky) into table \@ref(tab:bach-stravinsky-symmetric).

```{r,bach-stravinsky-symmetric, cache=TRUE}
b_o_t_s_symmetric <- tribble(
	   ~"", ~A, ~B,
	   "A", "$0, 0$", "$4, 1$",
	   "B", "$1, 4$", "$0, 0$"
	)
gameformat(b_o_t_s_symmetric, "Battle of the sexes, relabeled.")
```

After making that change, change column's payouts so that it is a demonic game. The result is table \@ref(tab:bach-demon)

```{r,bach-demon, cache=TRUE}
b_o_t_s_demonic <- tribble(
	   ~"", ~A, ~B,
	   "A", "$0, 1$", "$4, 0$",
	   "B", "$1, 0$", "$0, 1$"
	)
gameformat(b_o_t_s_demonic, "A demonic version of battle of the sexes.")
```

Finally, replace Demon's choices with states generated by (probably accurate) predictions, to get the decision problem in table \@ref(tab:asymm-death-damascus).

```{r,asymm-death-damascus, cache=TRUE}
asymm_d_i_d <- tribble(
	   ~"", ~PA, ~PB,
	   "A", "$0$", "$4$",
	   "B", "$1$", "$0$"
	)
gameformat(asymm_d_i_d, "A demonic decision problem based on battle of the sexes.")
```

That decision problem is the asymmetric version of Death in Damascus from @Richter1984.

The point of this section has not just been to show that we can turn games into decision problems by treating one of the players as a predictor. That's true, but not in itself that interesting. Instead I want to make two further points.

One is that most of the problems that have been the focus of attention in the decision theory literature in the past couple of generations can be generated from very familiar games, the kinds of games you find in the first one or two chapters of a game theory textbook. And the generation method is fairly similar in each respect.

The second point is that most of the simple games you find in those introductory chapters turn out to result, once you transform them this way, in demonic decision problems that have been widely discussed. But there is just one exception here. There hasn't been a huge amount of discussion of the demonic decision problem you get when you start with the game known as stag hunt. I'll turn to that in the next subsection.

In later parts of the book, I'll be frequently appealing to decision problems that are generated from other games that have been widely discussed by economic theorists. Most of these discussions are not particularly recent; the bulk of the work I'll be citing is from the 1980s and 1990s, and I don't take myself to be making a significant contribution to contemporary economic theorising. But what I want to point out is that there is a vast source of examples in the economic theory literature that decision theorists could be, and should be, discussing. And I've spent so long here on the translation between the two literatures in part because I think there are huge gains to be had from bringing these literatures into contact.

### An Indecisive Example {#indecisive}

This subsection is mostly going to be talking about games that are commonly known as stag hunts. Brian Skyrms has written extensively on why stag hunts are philosophically important [@Skyrms2001; @Skyrms2004], and putting them at the center of the story is one of several ways in which this book is following Skyrms's lead.

Stag hunts are symmetric two-player two-option, simultaneous move games. So they can be defined by putting constraints on the values in \@ref(tab:basic-sym-game). In this case, the constraints are

-   $x > z$
-   $w > y$
-   $x > w$
-   $z + w > x + y$

The name comes from a thought experiment in Rousseau's *Discourse on Inequality*.

> They were perfect strangers to foresight, and were so far from troubling themselves about the distant future, that they hardly thought of the morrow. If a deer was to be taken, every one saw that, in order to succeed, he must abide faithfully by his post: but if a hare happened to come within the reach of any one of them, it is not to be doubted that he pursued it without scruple, and, having seized his prey, cared very little, if by so doing he caused his companions to miss theirs.  [@Rousseau1913 209--10]

Normally, option A is called hunting, and option B is called gathering. The game has two equilibria - both players hunt, or both players gather. So it's unlike prisoners' dilemma, which only has one equilibria. And the more cooperative equilibria, where both players hunt, is better. But, and this is crucial, it's a risky equilibria. To connect it back to Rousseau, the thought is that the players would both be better off if they both cooperated to catch the stag (or deer in this translation). But cooperating is risky; if the players do different things, it is best to go off gathering berries (or bunnies) on one's own than trying in vain to catch a stag single-handed.

And that's what we see in the game. The first two constraints imply that the game is in equilibrium if the players do the same thing. The third constraint says that if they both hunt, option A, they are better off than if they both gather, option B. But the fourth constraint codifies the thought that this is a risky equilibrium. Even though the equilibrium where everyone hunts is better, there are multiple reasons we might end up at the equilibrium where everyone gathers.

One reason for this is that the players might want to minimise regret. Each play is a guess that the other player will do the same thing. If one plays A and guesses wrong, one loses $w - y$ compared to what one could have received. If one plays B and guesses wrong, one loses $x - z$. And the last constraint entails that $x - z < w - y$. So playing B minimises possible regret.

Second, one might want to maximise expected utility, given uncertainty about what the other player will do. Since one has no reason to think the other player will prefer A to B or vice versa - both are equilibria - maybe one should give each of them equal probability. And then it will turn out that B is the option with highest expected utility. Intuitively, B is a risky option and A is a safe option, and when in doubt, perhaps one should go for the safe option.

There are other arguments too for choosing to gather rather than hunt. To use a notion from Skyrms, gathering has a larger *basin of attraction* than hunting, and its basin of attraction includes the midpoint of the probability space. Those two motivations, plus the two from the previous two paragraphs, give us four motivations for gathering. If one wants to motivate gathering in general, it is actually important to get clear on which of the four one takes to be most important. Because while they are equivalent in the two option game (or problem), they are not in general equivalent. Indeed, all four come apart very soon once more options get added. Since I'm not defending gathering, I think decision theory should say either option is acceptable in stag hunts, I'm not going to continue exploring this line. But there are some interesting technical questions along these paths.

We can turn stag hunt into a decision problem by replacing the other player with Demon in a way that should be familiar by now. The result is the decision problem in table \@ref(tab:stag-decision).

```{r,stag-decision, cache=TRUE}
stag_decision <- tribble(
	   ~"", ~PA, ~PB,
	   "A", "$x$", "$y$",
	   "B", "$z$", "$w$"
	)
gameformat(stag_decision, "A demonic decision problem based on stag hunt.")
```

In order to have less algebra, I'm going to often focus mostly on a particular version of this decision, with the values shown in table \@ref(tab:stag-decision-particular). But it's important that the main conclusions will be true of all decision problems based on stag Hunt.

```{r,stag-decision-particular, cache=TRUE}
stag_decision_particular <- tribble(
	   ~"", ~PA, ~PB,
	   "A", "10", "0",
	   "B", "6", "8"
	)
gameformat(stag_decision_particular, "A particular version of a demonic decision problem based on stag hunt.")
```

These kinds of cases are important in the history of game theory because they illustrate in one game the two most prominent theories of equilibrium selection: risk dominance and payoff dominance [@HarsanyiSelten1988]. Risk dominance recommends gathering; payoff dominance recommends hunting. And most contemporary philosophical proponents of decisive decision theories (in the sense of decisiveness described back in section \@ref(procdef)) fall into one of these two camps.

In principle, there are three different views that a decisive theory could have about stag decisions: always hunt, always gather, or sometimes do one and sometimes the other. A decisive theory has to give a particular recommendation on any given stag decision, but it could say that the four constraints don't settle what that decision should be. Still, in practice all existing decisive theories fall into one or other of the first two categories.

One approach, endorsed for rather different reasons by Richard @Jeffrey1983 and Frank @Arntzenius2008, says to hunt because it says in decisions with multiple equilibria, one should choose the equilibria with the best payout. Proponents of Evidential Decision Theory also say to hunt in these situations, because the all-hunt outcome is better than the all-gather outcome, and it doesn't even matter whether these are equilibria. Another family of approaches says to always gather in stag decisions. For very different reasons, this kind of view is endorsed by Ralph @Wedgwood2013, Dmitri @Gallow2020 and Abelard @Podgorski2022. These three views differ from each other in how they motivate gathering, and in how they extend the view to other choices, but they all agree that one should gather in any stag decision.

I'm going to argue that all of these views are mistaken. Decision theory should not say what to do in these cases - either choice is rational.

Now I should note here that I'm slightly cheating in setting out the problem this way. The theory I defend says that in any decision problem like this with two equilibria, either choice can be rational. And that includes games like, say, the one in \@ref(tab:not-stag-decision), where everyone I mentioned in the last few paragraphs would agree that A is the uniquely correct choice.

```{r,not-stag-decision, cache=TRUE}
not_stag_decision <- tribble(
	   ~"", ~PA, ~PB,
	   "A", "10", "0",
	   "B", "2", "4"
	)
gameformat(not_stag_decision, "A multiple equilibrium decision problem that is not a stag hunt.")
```

I certainly don't want to lean too hard on the intuition that either option is rational in a stag hunt---though I do in fact think that it's intuitive that either option is rational in a stag hunt. But if we were just leaning on intuitions, then this last example would be devastating to my theory, since it really isn't particularly intuitive here that either option is rational. Thankfully, the argument, which I'll set out in some detail in chapter \@ref(decisive), doesn't appeal to these kinds of intuitions. Still, I think it's useful to focus on stag hunts because, as Skyrms shows, they are so philosophically important. And they will be my canonical example of a problem where the right decision theory is Indecisive.

### An Example of a Dilemma {#firstdilemma}

-   Rock Paper Scissors

## Plan for the Book {#Plan}

**NOT COMPLETE**

-   Talk about appendices
-   Talk about each chapter
-   This is literally the last thing to write

# Why So Defensive? {#defensive}

I am arguing for causal ratificationism. And much of the argument will come in the next three chapters, when I argue in turn against three of the component parts of of proceduralism. But in this chapter I want to first address an argument that proceduralism must be right, because only procedural theories can deliver what decision theory promises: a rule for making decisions. And the main argument of this chapter is going to be that decision theory cannot, and should not, be in the business of providing such a rule. Such a rule would have to be sensitive to resource constraints, and this kind of sensitivity isn't compatible with doing the kind of theorising that decision theorists do.

## Decision Theory and Making Decisions {#algorithm}

I'll get to why I think decision theory isn't, and shouldn't be, used to help humans make decisions. First I want to argue against something that is perhaps less widely believed, but it probably more plausible: that decision theory will be a helpful way for machines who are not subject to serious resource constraints to make decisions.

We are, collectively, currently engaged in a massive project of making machines that make decisions, from 'smart' thermostats to self-driving cars. Now one might have hoped that decision theory would have something useful to contribute to this project. That hope I think can be realised, but it's complicated. One might have further hoped that decision theory would be helpful in a way that only a proceduralist theory can be helpful, by providing an algorithm to program into the machines. And that hope that some might have won't be, and shouldn't be, met. That's because sometimes we want the machines to be irrational. Here is one simple case of this, based in part on David Lewis's work on nuclear deterrence [@Lewis1989c], and in part on *Dr. Strangelove* [@Kubrick1964].

Chooser is President of a relatively small country. Due to an unfortunate machine translation incident with their larger neighbor during widget tariff negotiations, the neighbor has become an enemy. (The fact that they are called Enemy should have been a clue that this would happen, but Chooser has the worst advisors and no one noticed this until it was too late.) And Enemy now plans to express their displeasure by launching a nuclear missile at chooser's largest city. Chooser doesn't have many ways to respond to this; any normal attempt at retaliation would just launch a larger war that would go very badly for Chooser's country.

Fortunately, Chooser's military has just developed a doomsday device. If launched, the doomsday device will kill everyone in Enemy's country. And Enemy is smart enough, or at least self-interested enough, to not do anything that will lead to the doomsday device being launched. Probably. Unfortunately, the doomsday device will not just kill everyone in Enemy's country, it will also kill everyone in Chooser's country. Fortunately, Chooser also has the ability to tie an automated launcher to the doomsday device, so it will launch if any nuclear missile hits their major city. And they have the ability to let Enemy know that they have tied an automated launcher to the doomsday device. So they can make a very credible threat to Enemy.

If it is likely enough that Enemy will back down when threatened this way, Chooser should install the automated launcher. And, and this is very important, they should make sure Enemy knows that they have done so. Even if there is some small probability $\varepsilon$ that Enemy will launch anyway, if $\varepsilon$ is small enough, and the probability of having everyone in the largest city killed high enough, it is a risk worth taking.

I'd originally thought of making the doomsday device kill not just everyone in Chooser's country and Enemy's country, but everyone in the world. (As was the case for the doomsday device in *Dr. Strangelove*.) But this complicates the decision making in ways I'd rather avoid. For one thing we have to account for the loss of future generations. For another, as Jonathan @Knutzen2022 points out, we have to account for the loss of humanity in general, on top of the loss of all those individual humans. Maybe there is no realistic probability of failure small enough that this could be a reasonable risk. But there surely is a probability of failure small enough that the risk of losing a whole country is worth trading off against the certainty of losing the largest city. And that's the risk I'm asking Chooser to take in this particular example. And I think in the right circumstances, it's a risk to take.

But now change the example in a few ways. Chooser still has the doomsday device, but they don't have the automated launcher. Fortunately, Enemy is now blessed, or cursed, with a Demon, who can predict with very high probability how Chooser will react if the largest city is destroyed. In particular, the Demon can predict, with very high probability, whether Chooser will react by launching the doomsday device, killing everyone in both countries. Unfortunately, the Demon seems to have predicted that Chooser will not do that, because the nuclear missile is now headed towards the largest city. What should Chooser do?

I think it's very plausible that Chooser should not respond by launching the doomsday device. Even if Chooser wants to punish Enemy country for launching the nuclear missile, which is a reasonable enough wish, the punishment would not be proportionate, and the damage to Chooser's own citizens would be intolerable. If Chooser's only options are the doomsday device or nothing, Chooser has to do nothing. Or so I think; I'll just note that this is an appeal to intuition about a case and that some people may feel differently. But let's explore what happens if you agree that it would be wrong to kill everyone in two countries to try and prevent a nuclear missile launch that's already happened.

One thing that follows from this view about Chooser is that rational choice is not the same thing as the choice a well designed machine would make. And, conversely, a well designed machine will not do just what the rational choice is. We just said that the right way to program the machine, if it is available, is to launch the doomsday device as soon as the nuclear missile is detected. But Chooser, if they are rational, will not launch the doomsday device in response to the nuclear missile. This is a counterexample to Functional Decision Theory (FDT), which says that the rational choice in a situation is the manifestation in that situation of the optimal algorithm [@LevinsteinSoares2020]. The optimal algorithm is the one the machine runs: automatically launch the doomsday device. But that's not what is rational.

The problem here is that launching the doomsday device is what game theorists call a non-credible threat. And you can't make a non-credible threat credible by loudly insisting that you'll really really do it this time, or even that it would be the rational thing to do.

There is another problem for FDT concerning pairs of cases. Change the second example so that Enemy's Demon is actually not very reliable. In this variant, they are better than chance, but not a lot better. Now Chooser certainly wouldn't set up the doomsday machine to automatically launch; the risk of a false positive is too high. So FDT says that in the variant where chooser has to decide what to do after learning the launch was made, chooser will do nothing. And while this is the right thing to do, it is made for the wrong reasons. Once Enemy has launched the missile, Chooser's best estimate yesterday of how reliable Demon in general was becomes irrelevant to what chooser should do. But according to FDT it could be decisive.

So if decision theory is relevant to building machines that make decisions, it's not because the right decision theory should be built into those machines. And hence it's not because decision theory must be proceduralist in order to make it possible to build it into these machines. It's rather because the people who make the machines face a very hard decision problem about what kinds of machines to build, and decision theory could be relevant to that problem. But how is decision theory relevant to that problem, or indeed any problem? The next section looks at that question in more detail.

## Why Do Decision Theory {#why}

What are we trying to do when we produce a decision theory? I think some of the disputes within the field come from different theorists having different motivations, and hence different answers to this question. My answer is going to be that decision theory plays a key role in a certain kind of explanatory project. And I think defensivism is well suited to play that role. But to see what I mean by playing a key role in an explanatory project, it helps to compare that with other possible views about the aim of decision theory.

One thing you might hope decision theory would do, and certainly one thing students often expect it will do, is provide advice on how to make decisions. I think decision theory is very ill-suited to this task, and it shouldn't really be the aim of the theory. The primary reason for this is that in any real life situation, the inputs are too hard to identify. To use decision theory as a guide to action, I need to know the utility of the possible states. And I need to know not just what's better and worse, but how much they are better or worse. At least speaking for myself, the only way I can tell the magnitudes of the differences in utilities between states is to ask about various gambles, and think about which of them I'd be indifferent between. That's to say, the only way I can tell that the utility of A is half way between that of B and C is to ask whether I'd be indifferent between A and a 50/50 chance of getting B or C. So I have to know what decisions I'd (rationally) make before I can work out the utilities. And that means I have to know what decisions to make before I can even apply decision theory, which is inconsistent with thinking that decision theory should be the guide to what decisions to make. This isn't such a pressing problem when decisions can be made using purely ordinal utilities, but those cases are rare. So in general there is little use for decision theory in advising decisions.

A somewhat better use for decision theory is in evaluation. Using decision theory, we can look at someone else's actions and ask whether they were rational. This is particularly pressing in cases where the person has harmed another, perhaps due to possible carelessness, or in defense of another, and we're interested in whether their actions were rational. Now one immediate complication in these cases is that we don't know the actor's value function. Even if the action doesn't maximise value as we see it, we don't know whether they have a different value function (perhaps one that puts low weight on harms to others), or they were doing a bad job maximising value. We don't know whether they were a knave or a fool. But it's often charitable to assume that they do have a decent enough value function, and we can ask whether what they were doing was rational if they did indeed have a decent value function.

This is a task decision theory is useful for, but it alone wouldn't justify the existence of books like this one. For one thing, the theory of how to act around Demons isn't usually relevant to whether an act was careless, or a permissible kind of self/other-defense. It is sometimes relevant. Sometimes we should think game-theoretically about whether a person was acting properly, and that will bring up issues that are similar to issues involved in making decisions around Demons. But usually the kind of decision theory we need in these cases is fairly elementary.

A bigger problem is that rationality is too high a bar in these cases. (I'm indebted here to Jonathan Sarnoff.) Imagine that D puts up a ladder somewhat sloppily, and it falls and injures V. The question at hand is whether D is morally responsible for the injury to V due to their carelessness. Ladders are tricky things, and sometimes one can take reasonable precautions and bad things happen anyway. Sometimes it is correct to attribute an injury to bad luck even if a super-cautious person would have avoided it. We aren't in general obliged to take every possible precaution to avoid injuring others. (If we were, we wouldn't be able to go out in public.) So what's the test we should use for whether this particular injury was just a case of bad luck or a case of carelessness? It is tempting to use decision theory here. The injury is a case of bad luck iff it was decision-theoretically rational for D to act as they did, assuming they had a decent value function. The problem is that this is a really high bar. Imagine that D did what any normal person would do in setting up the ladder, but there was a clever way to secure it for minimal cost that D didn't notice, and most people wouldn't have noticed. Then there is a good sense in which what D did was not decision-theoretically rational; the value maximising thing to do was the clever trick. But we don't want people to be morally responsible every time they fail to notice a clever option that only a handful of people would ever spot. And this is the general case. Decision-theoretic rationality is a maximising notion, and as such it's a kind of hard norm to satisfy. We don't want every failure to satisfy it in cases of unintentional injury to others, or intentional injury to others in the pursuit of a justifiable end like self-defence, to incur moral liability. So this isn't actually a place where decision theory is useful.

And if decision theory isn't useful in these cases, then it's value as an evaluative tool is somewhat limited. We can still use it for going around judging people, and saying that was rational, that was irrational. And that's a fine pastime, being judgmental can be fun, especially if the people being judged are in charge of institutions we care about. But we might hope for a little more out of our theory.

A third role for decision theory is in predicting what people will do. Sometimes we know people's incentives well enough to be able to predict that they will act as if they are rational. And at least sometimes, that can lead to surprising results. I'll talk through one case that I find surprising, and I suspect other philosophers will find surprising too.

Chooser runs a televised rock-paper-scissors tournament. Ratings are fine, but Chooser is told by the bosses that what the audience really likes is when rock beats scissors. The audience doesn't think rock-paper-scissors is really violent enough, and the implicit violence of rock smashing scissors is a help. So Chooser is thinking about generating more outcomes where rock beats scissors. And their plan is to make a cash payoff to players every time they successfully play rock, on top of the point they get for winning the game. Chooser's hope is that the game payoff will change from the standard payoff table, as shown in table \@ref(tab:simple-rps), to table \@ref(tab:adjusted-rps).

```{r, simple-rps, cache=TRUE}
# Burp
rps <- tribble(
	   ~"",         ~Rock,   ~Paper, ~Scissors,
	   "Rock",      "0, 0", "-1, 1",  "1, -1",
	   "Paper",    "1, -1",  "0, 0",  "-1, 1",
	   "Scissors", "-1, 1", "1, -1",   "0, 0"
	)
gameformat(rps, "Rock-Paper-Scissors")
```

```{r, adjusted-rps, cache=TRUE}
# Burp
rps_adjust <- tribble(
	   ~"",         ~Rock,   ~Paper, ~Scissors,
	   "Rock",      "0, 0", "-1, 1",  "2, -1",
	   "Paper",    "1, -1",  "0, 0",  "-1, 1",
	   "Scissors", "-1, 2", "1, -1",   "0, 0"
	)
gameformat(rps_adjust, "Rock-Paper-Scissors with Bonus for Rock")
```

They turn to their resident decision theorist to ask how much this will improve ratings. If you haven't worked through this kind of problem before, it's actually a fun little exercise to work out what the effect of this change will be. Because the disappointing news they are going to get from the decision theorist is that this move will backfire. After the change the rock-scissors combination will occur less often than it did before the change.

In the original game, it's pretty clear what the unique equilibrium of the game is. Each player plays each option with probability $\frac{1}{3}$. If either player had any deviation from that, then they would in the long run be exploitable. So that's what they will do, over a long enough run. And that means that a combination where one player chooses Rock and the other chooses Paper will occur in $\frac{2}{9}$ of games.

But what's the equilibrium of the new game? It's symmetric, each player uses the same mixed strategy. And in that mixed strategy, a player chooses Paper with probability $\frac{5}{12}$, Rock with probability $\frac{1}{3}$, and Scissors with probability $\frac{1}{4}$. So the combination where one player chooses Rock and the other chooses Paper will occur in $\frac{1}{6}$ of games, a considerable reduction from what it previously was.

It is very easy to share the intuition that if you reward a certain kind of behavior, you'll see more of it. (Some politicians, whose knowledge of economics starts and ends with supply-and-demand graphs, rely on nothing but this intuition in economic policy making.) But that intuition doesn't always work in the context of competitive games. Here rewarding Rock doesn't result in any change to how often Rock is played, but does result in a reduction of how often one plays the strategy that Rock defeats. What it actually incentivises is behavior that is outside this Rock-Scissors interaction, i.e., Paper. Now this doesn't require a huge amount of decision theory to work out - it's pretty simple linear algebra. But the intuition that rewarding a kind of behavior causes it to be more common is widespread enough that I think a theory that predicts it won't happen isn't completely trivial.

So cases like these are cases where, I think, decision theory has a useful predictive function. And this means it has practical advantages to the institutional designer, i.e., the chooser in this case. Knowing a bit of decision theory will tell them not to literally waste their money on this plan to reward players who win playing rock. Does it also have practical advantages to the players? Perhaps it has some, though it's a little less clear. After all, if every other player finds the new equilibrium quickly enough, then the expected return of each strategy in a given game will be equal. Decision theory itself says that in a particular play it doesn't matter what a player does. So it kind of undermines its own claim to being practically significant to the players. But this shouldn't reduce how useful the theory is at predicting how players will react to changes in the institutional design, and hence how valuable the theory could be to institutional designers.

But while decision theory can be predictively useful, the main role it plays is in explaining human behavior. Think about the explanation George Akerlof offers of why used cars lose value so quickly, i.e., why people don't pay nearly as much for lightly used cars as they play for new cars [@Akerlof1970]. Or think about the explanation Michael Spence offers for why employers might pay more to hire college graduates even if college does not make employees more productive [@Spence1973]. In each case carefully thinking about the decision problem each actor faces can give us a story about how behavior that looks surprising at first actually makes sense.

The point is not that these explanations always work. Both of them make substantive assumptions about the kind of situation that actors are in. I'm inclined to think that the assumptions in Akerlof's model are close enough to true that his explanation works, and the ones in Spence's model are not. But whether you think that's true or not, what you should think is that models like these show how decision theory can play a role in simple but striking explanations of otherwise mysterious behaviour.

A key assumption in each such explanation is that actors are basically rational. Or, at least, that their behaviour is close enough to what it would be if they were rational that rationality is a good enough assumption for explanatory purposes. A long tradition in philosophy of economics is that this is a fatal weakness in these explanations. After all, we know that people are not in fact perfectly rational. But I'm inclined to think it is a strength, in fact an important strength of decision theoretic explanations of behavior.

The fact that people are not perfectly rational does not mean that we cannot explain their behaviour using models that assume rationality. All that we need for that is that in a particular situation, the behaviour is as it would be if they were rational. And that can be true in certain domains. For example, people who prefer vanilla ice cream to strawberry ice cream buy more vanilla ice cream than strawberry ice cream. Now wheeling out a belief-desire model of action, combined with an assumption that ice-cream purchases are made by practically rational actors, to explain that pattern of purchases would be overkill. But it wouldn't be wrong. In some cases people collectively do act as they would if they were all rational. Not in all cases, of course, but in some. And to tell whether we are in such a case, we have to look.

To know whether people are acting rationally, we sometimes need to have a sophisticated theory of decision. At first glance, it might look irrational to have a very strong preference for new cars over lightly used cars. It takes some work to see that it might be, as Akerlof argued, a rational reaction to epistemic asymmetries. This work is a project that decision theory can contribute to, and indeed has contributed to.

The project of at least trying to see how surprising behaviour might be rational, a project which decision theory has a key role in, is valuable for two reasons.

One reason is epistemic. It's really easy to fall into thinking that certain behaviour is the result of a bias, and not even look for possible rational explanations of it. This is what Brighton and Gigerenzer call the 'bias bias' [@BrightonGigerenzer2015]. You don't even have to posit a bias in favor of explanations in terms of irrationality to get this result. Often the explanation in terms of irrationality is easier. It's much easier to say that people have an irrational attachment to new cars than to build a model of rational choice under epistemic asymmetry that explains the behaviour. And obviously its easier to settle for easy explanations. A commitment to looking for rational explanations of behaviour is a useful practice because it makes the researcher not settle for simple explanations. It might be that on a particular occasion the simple explanation is right, and people are just being irrational. But it is often a good use of time to at least look at what the best rational explanation is, and see if it is as plausible as the best irrational explanation.

Another reason is moral. There is a kind of respect involved in treating people as rational, or at least taking as a live option that people are acting rationally unless there is fairly strong reason to believe they are not. And we should show this kind of respect to other humans. And explanations of behaviour in terms of rational choice typically have the advantage that they make sense not just to the theorist, but to the person actually carrying out the behaviour. They allow for at least the possibility of the theorist and the person being theorised about to understand the behaviour in the same way. And that's a kind of equality that we should value.

The upshot of these considerations is that we should try to see how surprising behaviour could be rational. Rather than seeing someone as incompetently trying to carry out our ends, we should consider the possibility that behaviour we find surprising is the result of differences in what evidence is available, or in what values the actors have. Making sure we at least check what an explanation in terms of rational choice would look like is a useful heuristic because it sometimes turns up surprising and plausible models. It's an empirical question whether it is an efficient heuristic. I suspect it is, but maybe there are other heuristics that more efficiently lead to plausible models. Even if that were so, I would still think that it would be good to start by looking for rational choice models. That's because the moral reasons in favor of looking for these kinds of models, or for these kinds of explanations, would be decisive.

That's what I think the primary purpose of decision theory is. It's part of the project of trying to explain surprising aspects of human behaviour in terms of rational choices by people with different amounts of evidence, and different values. And since that's a very valuable project, I think decision theory is valuable, at least insofar as it contributes to the project.

For what it's worth, I think the kind of decision theory I'm defending, where the central principle is that choices must be defensible, has a much better track record of contributing to rational explanations of surprising behaviour than do its rivals. I don't know of real world situations in which we see Evidential Decision Theory play a particularly useful role in explaining what people do. Are there any papers based on EDT that as good as Akerlof's original paper on the market for lemons? Even if there are no such papers, it is possible that there are institutional reasons for this. Maybe not enough economists or political scientists get taught EDT, or maybe malicious journal editors conspire to not publish papers using models based on EDT. But if the role of decision theory is, as I've argued, to contribute to these kinds of explanations, it would be useful to see how much of a contribution to rival philosophical theories of decision could actually make.

## Why Do Ideal Decision Theory {#ideal}

The previous section was on why philosophers should care about decision theory. But what I'm doing in this book isn't just decision theory, it's a very specific kind of decision theory. What I'm doing might be called ideal decision theory. It's the theory of how idealised agents make decisions. And I'm going to appeal to those idealisations a fair bit in what follows. This section is about why we should care about such an idealised theory, and in particular about why a ratificationist decision theory should care about it.

One lesson of cases like **Salesman** is that every theory of idealised decision making needs to be complemented with a theory of non-ideal decision making. But this isn't the only lesson one could take from the example. Another lesson could be that ideal decision theory is a pointless enterprise. It should not be supplemented by non-ideal decision theory, but replaced. I don't think that's right, but it takes some argument to say why it isn't right. That longer argument will come in chapter \@ref(dilemma), but for now I'll just say what positive role I think ideal decision theory has to play.

Let's start with some things that ideal theory cannot do. It can't give people a target they should approximate. That's because the following is a very bad argument.

1.  The ideal is X.
2.  So, Chooser should be as much like X as they can be.

We know that isn't right for reasons set out by @LipseyLancaster1956. If one can't be like the ideal, it is often best to do other things that the ideal chooser would not do to offset these failings. Here's one simple example. The ideal chooser, in decision theory, can do all the reasoning that is needed for a problem instantaneously. So it's a bad idea for them to stop and have a think about it before making a big decision. Since they have thought all the thoughts that are needed, that would just be a waste of time. But it's often a very good idea for Chooser to stop and have a think about it before making a big decision. Not doing that, in order to be more like the ideal, is a mistake.

The following argument isn't as bad, but it isn't right either.

1.  The ideal is X.
2.  Chooser's situation is approximately ideal.
3.  So Chooser should do approximately X.

The situations where this fails are a bit more contrived than the situations where the previous argument failed, at least for typical individuals. But here the details of Lipsey and Lancaster's argument matter. At least when Chooser is designing institutions, like market structures or taxation systems, it turns out to very often be the case that the the second best solution looks dramatically different to the best solution. And someone who is approximately ideal might only be able to find the second best solution, not the best one, in a reasonable time. So it's possible that someone with very mild computational limitations to do something very different from what the ideal agent would do, and yet be acting optimally given their limitations.

The following two arguments, however, are good arguments. And the two main uses of ideal decision theory are related to these two good arguments.

1.  The ideal is X.
2.  The differences between Chooser's situation and the ideal are irrelevant.
3.  So Chooser should do X.

Ideal theory can provide advice, in situations that are like the ideal in suitable ways. It isn't trivial for the ideal theory to provide such advice though. The second premise is often very hard to justify. But in some cases it is not that hard---the computations that have to be made are easy, and the stakes are high, so it is worth spending the resources to make all the computations. And in those cases, we expect Chooser to act like the ideal. "Expect" here has both a normative and a descriptive meaning, and let's make the latter of those explicit with another good argument that uses ideal decision theory.

1.  The ideal is X.
2.  The differences between Chooser's situation and the ideal are irrelevant.
3.  So, Chooser will do X.

If we interpret 'situation' in premise 2 to include the fact that Chooser is (approximately) rational, then this is a good argument too. And when we have an argument like this, we can use it to predict what Chooser will do, and explain what Chooser has done.

This kind of argument can be used in explanations that have the structure Michael @Strevens2008 argues that explanations involving idealisations always have. They include a model saying what would happen in idealised situations. And they include a premise that doesn't just say that the real situation is close to the ideal, but that the differences don't matter for the purposes of what we're trying to explain. That kind of structure covers simple explanations using the ideal gas law you might learn in introductory chemistry, and it also covers explanations using ideally rational agents. What we need for, e.g., Akerlof's explanation of used car prices to work is not that everyone in that market is perfectly rational, but that they are close enough to rational for the purposes of predicting how they will behave in the used car market. That's important because participants in the used car market are not perfectly rational; they might not even be close to it. But just as real molecules can be modeled by things that are infinitely smaller than them, real buyers and sellers of used cars can be modeled by actors that are infinitely more rational than they are.

And that's the big picture project that I want this book to be contributing to. There are both epistemic and moral reasons to look for explanations of behaviour in terms of individuals acting rationally. These explanations will be idealised explanations. Idealised explanations involve describing carefully how things work in the ideal, and arguing that the differences between the ideal and the reality are unimportant for the particular thing being explained. The latter task is hard, and often not done with sufficient care, but isn't impossible. This book contributes primarily to the former task, though especially in chapter \@ref(dilemma) I'll have things to say relevant to the latter task as well.

David Lewis gives a similar account of the purpose of decision theory in a letter to Hugh Mellor. The context of the letter, like the context of this section, is a discussion of why idealisations are useful in decision theory. Lewis writes,

> We're describing (one aspect of) what an ideally rational agent would do, and remarking that somehow we manage to approximate this, and perhaps -- I'd play this down -- advising people to approximate it a bit better if they can. [@Lewis1981Mellor, 432]

To conclude this section on an historical note, I want to compare the view I'm adopting to the position Frank Knight puts forward in this famous footnote.

> It is evident that the rational thing to do is to be irrational, where deliberation and estimation cost more than they are worth. That this is very often true, and that men still oftener (perhaps) behave as if it were, does not vitiate economic reasoning to the extent that might be supposed. For these irrationalities (whether rational or irrational!) tend to offset each other. The applicability of the general "theory" of conduct to a particular individual in a particular case is likely to give results bordering on the grotesque, but *en masse* and in the long run it is not so. The *market* behaves *as if* men were wont to calculate with the utmost precision in making their choices. We live largely, of necessity, by rule and blindly; but the results approximate rationality fairly well on an average. [@Knight1921 67n1]

Like Knight, I think that explanations in social science can treat people as rational even if they are not, even if it would "give results bordering on the grotesque" to imagine them as perfectly rational. That's because, at least in the right circumstances, the irrationalities are irrelevant, or they cancel out, and the "as if" explanation goes through. Now I do disagree with the somewhat blithe attitude Knight takes towards the possibility that these imperfections will not cancel out, that they will in fact reinforce each other and be of central importance in explaining various phenomena. But that's something to be worked out on a case-by-case basis. We should not presuppose in advance either that the imperfections be irrelevant or that they will be decisive.

There is one other point of agreement with Knight that I want to emphasise. If we don't act by first drawing Marshallian curves and solving optimisation problems, how do we act? As he says, we typically act "by rule". Our lives are governed, on day-by-day, minute-by-minute basis, by a series of rules we have internalised for how to act in various situations. The rules will typically have some kind of hierarchical structure - do this in this situation unless a particular exception arises, in which case do this other thing, unless of course a further exception arises, in which case, and so on. And the benefit of adopting rules with this structure is that they, typically, produce the best trade off between results and cognitive effort.

One other useful role for ideal decision theory is in the testing and generation of these rules. We don't expect people who have to make split-second decisions to calculate expected utilities. But we can expect them to learn some simple heuristics, and we can expect theorists to use ideal decision theory to test whether those heuristics are right, or whether some other simple heuristic would be better. This kind of approach is very useful in sports, where athletes have to make decisions very fast, and there is enough repetition for theorists to calculate expected utilities with some precision. But it can be used in other parts of life, and it is a useful role for ideal decision theory alongside its roles in institutional design (as in the rock-paper-scissors example), and in explanation (as in the used cars example).

## Why Not Proceduralism {#whynot}

Let's take stock. This chapter has been a response to the following two kinds of worries.

-   The best thing that decision theory could do would be to provide a procedure for making good decisions.
-   If decision theory can't do that, it's a pointless activity.

So far the attention has been primarily on the second point. I've argued that it isn't true - that decision theory can have an important role in explanations of social phenomena even if it doesn't provide a procedure for making good decisions. But what about the first point? Even if this is a role for decision theory, wouldn't a procedure for good decisions be better? In some sense perhaps it would be, but there is no reason to think that anything like ideal decision theory is going to be part of such a procedure for creatures like us. For just one example, the optimal procedure in cases like **Salesman** will give advice that contradicts what every theory of ideal decision on the market says.

In practice, the best thing a decision theory can do is be part of a broader project of helping us understand and navigate the world. To do that, it need not provide a procedure that can be followed on any given occasion, and indeed it could not do that. It is better to provide tools that can be used in one or other multi-stage process. For instance, it can provide reasons for thinking that this or that institutional design will fail or succeed in a particular way. Or it can compare heuristics that humans are capable of applying. Neither of these roles require that decision theory be proceduralist. Indeed, a non-proceduralist theory that says that we can't predict how a certain institution will work, because it generates a decision problem for individuals with multiple solutions, might be more useful than one that posits a false certainty about what will happen. So, in short, there is no argument from the purpose to which decision theory can or should be put to the conclusion that decision theory should be proceduralist.

Nothing in this chapter, however, is an argument against proceduralism. It has been a long argument against the necessity of proceduralism, but nothing more. In the next two chapters, I will argue directly that the right decision theory is not proceduralist.

# Against Decisiveness {#decisive}

At the heart of causal ratificationism is the claim that in many decision problems, there is no uniquely rational solution. And this isn't because the options are tied. Rather, it is because each of them is acceptable once it is made. Causal ratificationism is *indecisive*; it doesn't always provide a verdict on what to do.

This chapter argues that the right decision theory, whatever it is, is indecisive. Or, to be a little more precise, it will be an argument that the right theory, whatever it is, is *weakly indecisive*. As I'll define the terms, causal ratificationism is *strongly indecisive*, and the argument of this chapter won't entail that the right theory should be strongly indecisive. In fact, in the whole book I'm not going to offer any kind of proof that the right theory is strongly indecisive. Instead, I'll argue for a number of constraints, including weak indecisiveness, and argue that causal ratificationism is the best way to satisfy those constraints. The most distinctive of these constraints is weak indecisiveness, and the point of this chapter is to motivate that constraint.

The last two paragraphs used a lot of terminology, and section \@ref(decisive-terms) clarifies and defines the key terms. Then in sections \@ref(decisive-games)--\@ref(decisive-examples), I'll argue that the right theory, whatever it is, is weakly indecisive. In section \@ref(decisive-theories) I'll show how this causes problems for most existing decision theories in philosophy, and compare it to existing objections to some of those theories. Finally in section \@ref(decisive-further), I'll discuss how these considerations relate to the defence of the view I really believe: that the right decision theory is strongly indecisive.

## What Is Decisiveness {#decisiveness-terms}

I will say a decision theory is **decisive** iff for any decision problem, it says either:

1.  There is a uniquely best choice, and rationality requires choosing it.; or
2.  There is a non-singleton set of choices each of which is tied for being best, and each of which can be permissibly chosen.

A decision theory is **decisive over binary choices** iff it satisfies this condition for all decision problems where there are just two choices. Most decision theories in the literature are decisive, and of those that are not, most of them are at least decisive over binary choices. I'm going to argue that the correct decision theory, whatever it is, is indecisive. It is not, I'll argue, even decisive over binary choices.

That definition, unfortunately, relies on two more terms that are not easy to define: *decision problem* and *tie*. I'll deal with these in reverse order.

For decisiveness to be anything other than a trivial truth, it can't just be that options are tied if each is rationally permissible. If that is what it meant for options to be tied, a decisive theory would just be one that either says one option is mandatory or many options are permissible. To make decisiveness a substantive claim, a different account of what it is for options to be tied is needed. I'll borrow a technique from Ruth @Chang2002 to provide such an account Some options are **tied** iff either is permissible, but this permissibility is sensitive to sweetening. That is, if options $X$ and $Y$ are tied, then for any positive $\varepsilon$, the agent prefers $X + \varepsilon$ to $Y$. If both $X$ and $Y$ are permissible, and this dual permissibility persists if either is 'sweetened', i.e., replaced by an option that is improved by $\varepsilon$, then they aren't tied. My thesis, the thesis that the right theory is indecisive, is that the right decision theory says that sometimes there are multiple permissible options, and each of them would still be permissible if one of them was sweetened.

I'm going to provide two notions of a decision problem, one concrete and one abstract. These will correspond to the two notions of decisiveness, weak and strong, that I've already mentioned.

First, the abstract sense. It suffices to specify an abstract decision problem to describe the following four values. (Note that this is stipulative; I'm hereby defining what I mean by *abstract decision problem*.)

-   What choices the chooser has;
-   What possible states of the world there are (where it is understood that the choices of the chooser make no causal impact on which state is actual);
-   What the probability is of being in any state conditional on making each choice; and
-   What return the chooser gets for each choice-state pair.

Most recent papers on decision theory do not precisely specify what they count as a decision problem, but they seem to implicitly use an account like this. It is very common in philosophical work on decision theory to see a vignette that settles nothing beyond these four things, and then the writer assumes that this or that decision theory should have something to say about the problem. (Commonly they will also make firm pronouncements about the intuitively right answer to this problem, and wield this as evidence that said decision theory is false.) So while this is stipulative, I don't think it's particularly distinctive; most theorists think of decision problems as something like this.

But look how much it leaves out! It says nothing about what time of day it is, what the weather is, how happy the chooser is feeling (unless this impacts the *returns* in the relevant sense), or many other things. Maybe those matter to decision theory. Maybe the right decision theory is CDT in the seminar room, and EDT in the pub, plus I guess a classification of possible situations into being more pub-like or seminar-room-like. Then a decision problem needs a fifth clause, which specifies whether the chooser is in a pub or a seminar room. At the very least, I think we should have a language for discussing decision theory that lets CDT-in-the-seminar-room/EDT-in-the-pub be a statable theory.

To that end, say a concrete decision problem is a centered world that has a chooser at its center. The centered world will determine an abstract decision problem. The function from concrete decision problems to abstract decision problems is not trivial. What, for instance, does it mean to say that these are, and these are not, the choices available to a concrete chooser? But I assume there is some function. There is no function in the reverse direction; every abstract decision problem corresponds to many, many concrete decision problems.

A decision theory is strongly decisive iff it is decisive over abstract decision problems. That is, it is strongly decisive iff for any abstract decision problem, it says that either there is a unique rational choice in the problem, or that some options are tied. A decision theory is weakly decisive iff it is decisive over concrete decision problems. That is, it is strongly decisive iff for any concrete decision problem, it says that either there is a unique rational choice in the problem, or that some options are tied. A decision theory is weakly indecisive iff it is not strongly decisive, and strongly indecisive iff it is not weakly indecisive.

Any strongly indecisive theory is weakly indecisive, but the converse is false. The CDT-in-the-seminar-room/EDT-in-the-pub theory is weakly indecisive. If it is presented Newcomb's Problem, it does not issue a verdict. It says both options are rationally consistent with the abstract structure of the problem. So it is not strongly decisive, which is to say it is weakly indecisive. But it is weakly decisive. In any concrete instance of Newcomb's Problem, the chooser is either in the seminar room (or a seminar-room-like space) or the pub (or a pub-like-space), and so there is only one rational choice. So it is not strongly indecisive.

The example I've used so far of a weakly but not strongly indecisive theory is not, I suspect, one that will appeal to many readers. There are, however much more interesting weakly decisive but not strongly indecisive theories. They will have to wait; for the next few sections, the focus will be on strongly decisive theories, and the development of an argument against them.

## Six Decision Problems {#decisive-games}

The core of this chapter, and indeed the core of this book, revolves around the decision problems in tables \@ref(tab:abc-v1) and \@ref(tab:abc-v6). I'm going to argue that whichever moves are acceptable in one of those two problems are also acceptable in the other, for any real values of $x_1, x_2, x_3, x_4, e$, and any value of $p \in (0, 1)$. The names of the problems are because they are going to be the first and sixth members of a sequence of problems that I'll present shortly.

```{r,abc-v1, cache=TRUE}
generic_abc_v1 <- tribble(
	   ~"", ~PU, ~PD,
	   "U", "$x_1$", "$x_2$",
	   "D", "$x_3$", "$x_4$"
	)
gameformat(generic_abc_v1, "Problem 1.")
```

```{r,abc-v6, cache=TRUE}
generic_abc_v6 <- tribble(
	   ~"", ~PU, ~PD,
	   "U", "$px_1 + (1-p)e$", "$x_2$",
	   "D", "$px_3 + (1-p)e$", "$x_4$"
	)
gameformat(generic_abc_v6, "Problem 6.")
```

Once I've argued that these problems should be treated the same way, I'll then argue that no strongly decisive theory does treat them the same way, so all strongly decisive theories are false. But what are these problems? What do these tables mean?

In each case, Chooser has two options: U for Up, and D for Down. There is a demon, called Demon, who is arbitrarily good at predicting Chooser's choices, but who is not causally influenced by what Chooser does.[^demons-and-decisions-6] Demon will select either PU, meaning they predict Chooser will play U, or PD, meaning they predict Chooser will play D. The two two-way choices produce four possible outcomes, and the payouts to Chooser in each of those four possibilities are shown in the table. So these are just the kinds of problems that are familiar in the modern decision theory literature. What's not familiar is the claim that Problem 1 and Problem 6 should be treated the same way. The argument for that goes via four more problems, and this section introduces them.

[^demons-and-decisions-6]: For simplicity, I'm going to assume that the probability that Demon predicts correctly is 1. If you don't want to allow that this probability can be 1 without having a causal connection, it won't make a huge difference to make it $1 - \varepsilon$. It's just important to note in Problems 4-6 that Demon acts as if the probability is 1. Even if there is some chance of Demon being wrong, Demon acts as if this isn't possible. This is not a distinctive assumption; in most equilibrium concepts in game theory, an equilibrium involves the players believing that their predictions of the other players are correct with probability 1.

**Problem 2** is just like Problem 1, except it provides more backstory about why Demon is so likely to correctly predict Chooser correctly. It is a game with two players: Chooser and Demon. Chooser's payouts are just as before, Demon gets zero payout for an incorrect prediction, and a positive payout for predicting correctly. And Demon is a very good predictor and an expected utility maximiser, so they will almost certainly make correct predictions. But note that Demon's payouts are not the same in each case of a correct prediction. Because Demon is playing a game not making a prediction, I've relabeled their moves. They are called B and C; option A will come in at the next step. Table \@ref(tab:abc-v2) is the game table for Problem 2.

```{r,abc-v2, cache=TRUE}
generic_abc_v2 <- tribble(
	   ~"", ~B, ~C,
	   "U", "$x_1, 1$", "$x_2, 0$",
	   "D", "$x_3, 0$", "$x_4, 2$"
	)
gameformat(generic_abc_v2, "Problem 2 (table version).")
```

In standard presentations of problems like Problem 1, it is assumed that Demon moves first, but the move isn't revealed until after Chooser moves. So while these problems are evidentially like simultaneous move games, there is a sense in which it is more accurate to represent them as game trees. So let's include the game tree version of Problem 2 as well, as figure \@ref(fig:abc-v2-tree).

```{tikz, abc-v2-tree, fig.cap = "Problem 2 (tree version).", fig.ext = 'png', cache=TRUE, fig.width = 4, fig.height = 4, fig.align="center"}
\usetikzlibrary{calc}
\begin{tikzpicture}[scale=1.5,font=\footnotesize]
\tikzset{
% Two node styles for game trees: solid and hollow
solid node/.style={circle,draw,inner sep=1.5,fill=black},
hollow node/.style={circle,draw,inner sep=1.5},
square node/.style={rectangle,draw, inner sep = 1, fill = black}
}
% Specify spacing for each level of the tree
\tikzstyle{level 1}=[level distance=20mm]
\tikzstyle{level 2}=[level distance=15mm,sibling distance=15mm]
\tikzstyle{level 3}=[level distance=15mm,sibling distance=15mm]
\tikzstyle arrowstyle=[scale=1]
\tikzstyle directed=[postaction={decorate,decoration={markings,
mark=at position .5 with {\arrow[arrowstyle]{stealth}}}}]
% The Tree
\node(0)[hollow node,label=above:{$Demon$}]{}

%child[grow=left, level distance=25mm]{node(1)[square node, label=left:{$e,1$}]{}
%edge from parent node[above]{A}
%}
child[grow=225]{node(3)[solid node]{}
child[grow=240]{node[square node,label=below:{$x_1,1$}]{}edge from parent node[left]{U} }
child[grow=300]{node[square node,label=below:{$x_3,0$}]{} edge from parent node[right]{D}}
edge from parent node[left, xshift = -3]{B}}
child[grow=315]{node(4)[solid node]{}
child[grow=240]{node[square node,label=below:{$x_2,0$}]{}edge from parent node[left]{U} }
child[grow=300]{node[square node,label=below:{$x_4,2$}]{} edge from parent node[right]{D}}
edge from parent node[right,xshift = 3]{C}
}
;
% information set
\draw[dashed](3) to (4);
% specify mover at 2nd information set
\node at ($(3)!.5!(4)$) [above] {$Chooser$};
\end{tikzpicture}
```

I don't know of any argument for treating the table version and tree version of Problem 2 differently, so I will follow the standard assumption that these are two representations of essentially the same problem.

**Problem 3** is just like Problem 2, except Demon is offered an exit strategy. When Demon moves, they can now choose A, B or C. If they choose B or C, the game continues as before. Chooser is told that Demon chose B or C, but not which one they chose, and knows all the payouts, and has to choose U or D. If Demon chooses A, however, the game ends, and Chooser gets payout $e$, while Demon gets payout 1. Figure \@ref(fig:abc-v3) is the tree for that game.

```{tikz, abc-v3, fig.cap = "Problem 3.", fig.ext = 'png', cache=TRUE, fig.width = 5, fig.height = 5, fig.align="center"}
\usetikzlibrary{calc}
\begin{tikzpicture}[scale=1.5,font=\footnotesize]
\tikzset{
% Two node styles for game trees: solid and hollow
solid node/.style={circle,draw,inner sep=1.5,fill=black},
hollow node/.style={circle,draw,inner sep=1.5},
square node/.style={rectangle,draw, inner sep = 1, fill = black}
}
% Specify spacing for each level of the tree
\tikzstyle{level 1}=[level distance=20mm]
\tikzstyle{level 2}=[level distance=15mm,sibling distance=15mm]
\tikzstyle{level 3}=[level distance=15mm,sibling distance=15mm]
\tikzstyle arrowstyle=[scale=1]
\tikzstyle directed=[postaction={decorate,decoration={markings,
mark=at position .5 with {\arrow[arrowstyle]{stealth}}}}]
% The Tree
\node(0)[hollow node,label=above:{$Demon$}]{}

child[grow=left, level distance=25mm]{node(1)[square node, label=left:{$e,1$}]{}
edge from parent node[above]{A}
}
child[grow=225]{node(3)[solid node]{}
child[grow=240]{node[square node,label=below:{$x_1,1$}]{}edge from parent node[left]{U} }
child[grow=300]{node[square node,label=below:{$x_3,0$}]{} edge from parent node[right]{D}}
edge from parent node[left, xshift = -3]{B}}
child[grow=315]{node(4)[solid node]{}
child[grow=240]{node[square node,label=below:{$x_2,0$}]{}edge from parent node[left]{U} }
child[grow=300]{node[square node,label=below:{$x_4,2$}]{} edge from parent node[right]{D}}
edge from parent node[right,xshift = 3]{C}
}
;
% information set
\draw[dashed](3) to (4);
% specify mover at 2nd information set
\node at ($(3)!.5!(4)$) [above] {$Chooser$};
\end{tikzpicture}
```

That would be enough to specify a game, but decision theorists typically want something more. If Demon predicts Chooser will play D, then Demon will clearly play C. But what will happen if Demon predicts Chooser will play U? In that case, Chooser will get payout 1 from either A or B, and all we've been told is that Demon maximises expected utility. So let's add one specification to the game. If Demon predicts that Chooser will play U, they will play B with probability $p$, and A with probability $1-p$.

Note that this suggests Demon might not be ideally rational. From Demon's perspective, A weakly dominates B. But they have some probability of choosing B. If one thinks that ideal rationality requires eschewing weakly dominated options, this means Demon is not ideally rational. It was, however, never part of the story that Demon is ideal; just that they are a utility maximiser. And they can be, even if they choose B.

**Problem 4** is just like Problem 3, except Chooser must select before being told whether Demon has adopted the exit strategy of A. Imagine, to make it vivid, that the game-master is impatiently waiting for Demon's selection, but Demon is stuck in their room, taking their time. Chooser has to run, so the game-master says that Chooser should write their move in an envelope. If Demon chooses A, the envelope will be burned, since it doesn't matter . If Demon chooses B or C, the envelope will be opened and what's written in it will be Chooser's move. Because Chooser doesn't know which move Demon has made, the tree (shown in figure \@ref(fig:abc-v4)) looks a little different.

```{tikz, abc-v4, fig.cap = "Problem 4 (tree version).", fig.ext = 'png', cache=TRUE, fig.width = 6, fig.height = 6, fig.align="center"}
\usetikzlibrary{calc}
\begin{tikzpicture}[scale=1.5,font=\footnotesize]
\tikzset{
% Two node styles for game trees: solid and hollow
solid node/.style={circle,draw,inner sep=1.5,fill=black},
hollow node/.style={circle,draw,inner sep=1.5},
square node/.style={rectangle,draw, inner sep = 1, fill = black}
}
% Specify spacing for each level of the tree
\tikzstyle{level 1}=[level distance=15mm,sibling distance=25mm]
\tikzstyle{level 2}=[level distance=10mm,sibling distance=13mm]
\tikzstyle{level 3}=[level distance=15mm,sibling distance=15mm]
\tikzstyle arrowstyle=[scale=1]
\tikzstyle directed=[postaction={decorate,decoration={markings,
mark=at position .5 with {\arrow[arrowstyle]{stealth}}}}]
% The Tree
\node(0)[hollow node,label=above:{$Demon$}]{}

child{node(2)[solid node]{}
child{node[square node,label=below:{$e,1$}]{}edge from parent node[left]{U} }
child{node[square node,label=below:{$e,1$}]{} edge from parent node[right]{D}}
edge from parent node[left, xshift = -4]{A}}
child{node(3)[solid node]{}
child{node[square node,label=below:{$x_1,1$}]{}edge from parent node[left]{U} }
child{node[square node,label=below:{$x_3,0$}]{} edge from parent node[right]{D}}
edge from parent node[left, xshift = 0]{B}}
child{node(4)[solid node]{}
child{node[square node,label=below:{$x_2,0$}]{}edge from parent node[left]{U} }
child{node[square node,label=below:{$x_4,2$}]{} edge from parent node[right]{D}}
edge from parent node[right,xshift = 4]{C}
}
;
% information set
\draw[dashed](2) to (4);
% specify mover at 2nd information set
\node at ($(3)!.3!(4)$) [above] {$Chooser$};
\end{tikzpicture}
```

Since Demon has no evidence of Chooser's move when Demon makes their one move, and Chooser has no evidence of Demon's move when Chooser makes their move, this is effectively a one-shot simultaneous move game. So we can represent it just as well with table \@ref(tab:abc-v4-table) as a tree.

```{r,abc-v4-table, cache=TRUE}
generic_abc_v4 <- tribble(
	   ~"", ~A, ~B, ~C,
	   "U", "$e, 1$", "$x_1, 1$", "$x_2, 0$",
	   "D", "$e, 1$", "$x_3, 0$", "$x_4, 2$"
	)
gameformat(generic_abc_v4, "Problem 4 (table version).")
```

Since Demon is an external force that produces states of the world which Chooser has no causal influence over, we can just as well treat Demon's moves as states. That will get us **Problem 5**, as shown in table \@ref(tab:abc-v5). In that problem, the three moves from Problem 4 become three states. And Chooser knows that $Pr(C | D) = 1, Pr(B | U) = p, Pr(A | U) = 1-p$, and all the other conditional probabilities of states given choices are set to zero.

```{r,abc-v5, cache=TRUE}
generic_abc_v5 <- tribble(
	   ~"", ~A, ~B, ~C,
	   "U", "$e$", "$x_1$", "$x_2$",
	   "D", "$e$", "$x_3$", "$x_4$"
	)
gameformat(generic_abc_v4, "Problem 5.")
```

In this problem, the two states A and B are separated out. But these are just two possible ways that the state PU could be realised. We could collapse these states into one, and replace the listed payouts with Chooser's expected payouts, given that PU is realised and they make a particular choice. That will give us **Problem 6**, with the following familiar looking table.

```{r,abc-v6-reprise, cache=TRUE}
gameformat(generic_abc_v6, "Problem 6 (reprise).")
```

So those are our six problems. In the next section, I'll argue that as theorists, we should think of all these problems the same way.

# Against Rational Possibility {#dilemma}

This chapter concerns dilemmas in ideal rational choice theory. A *dilemma*, as I'll use the term, is a situation where no choice is ideal. That is to say, it is a situation where there is nothing that the chooser can do which is ideal. The term *can* here is tricky, and we need to pause over it for a moment. It is an *agentive modal*, and there has been some interesting work recently showing how puzzling these are. [@MandelkernEtAl2017; @Schwarz2020] Here is one familiar case from that literature.

> **Bank**\
> Chooser is at an ATM and would like to withdraw cash, but has forgotten the PIN. In fact, it is 2203, but Chooser doesn't remember this. Typing in 2203 would let Chooser withdraw cash, which would be good, but typing in anything else would lead to the ATM card being confiscated by the machine, which would be bad. What should Chooser do?

One answer that is simple, elegant, and wrong is that Chooser should simply type in the correct PIN. That isn't right because Chooser can't type in the PIN; they've forgotten it. But, and here is the philosophical point I want us to focus on, there is a good sense in which they can type in the PIN. They can type 2, then 2, then 0, then 3, and to do that is to type in the PIN. So what happened to the view that Chooser can't type in the correct PIN?

It helps here to borrow a conceptual tool from @MandelkernEtAl2017. It is tempting to think of these agentive modals as possibility claims. To say that X can do something is to say that it is possible that X does it, perhaps under the right circumstances. They argue that this is wrong, and in fact they are necessity claims. To say that X can do something is to say that there are basic actions within X's power which necessitate that the thing happens. They go on to argue that this is what agentive modals mean in ordinary language, and I'm a little sceptical of that. I think the best pitchers in baseball can strike out big league hitters, but there are no basic actions those pitchers can perform which necessitate a strikeout. (If there was, you would wonder why they didn't do it every time.) But whatever we think about the analysis of ordinary language, the idea that there are both possibility and necessity readings of these claims is a helpful one. Because this is the key to understanding what's going on in **Bank**.

Can Chooser select the right PIN? On the possibility reading of *can*, the answer is yes. It is possible for Chooser to type 2,2,0,3. On the necessity reading, the answer is no. Even if Chooser typed those numbers in, it would be (at least from Chooser's perspective, which is what matters here) a fluke that it worked.

Which of these readings is relevant to whether a situation is a dilemma? Both of them. A dilemma is a situation is which for anything that the agent can do, in the necessity sense, there is something better that they can do, in the possibility sense. And the alternative action is better, in the relevant sense, if it has a higher expected return than the first action once the first action is performed. In this sense there are lots of dilemmas around.

There are, however, slightly more dilemmas given causal ratificationism than given some other theories. In particular, causal ratificationism says that there can be dilemmas even when (a) the chooser is ideally rational, and (b) the chooser only has finitely many choices. As we'll see, if either of these constraints are dropped, then every theory agrees that there can be dilemmas. This difference, however, leads to the first of the three arguments against causal ratificationism that are going to be running through the background of this chapter.

1.  The right theory of decision should not say that there can be dilemmas when choosers are ideally rational and face finitely many choices.
2.  Causal ratificationism says that there can be dilemmas when choosers are ideally rational and face finitely many choices.
3.  Therefore, causal ratificationism is false.

Here is a version of this argument from Arif @Ahmed2012, discussing a problem he named the ABZ problem.

> \[This\] is the catastrophic conclusion that whatever you do in ABZ is irrational: whichever option you take, some other option is (and always was) rationally preferable to it. It may be fair to say this about an agent whose beliefs or preferences are insane or incoherent and also about one whose options are infinite. But we are not facing either case here. Your beliefs and preferences in ABZ are, despite their science-fictional character, plainly sane and coherent. And it is equally clear that ABZ does not offer an infinitude of options but only three.

My response here, which might seem a little unsubtle, is simply to deny the philosophical relevance of the finite/infinite distinction in this case. If we can live with dilemmas in infinite cases, we can live with them in finite cases. Indeed, whatever we do to manage the dilemmas in infinite cases can be applied to manage them in finite cases. Ahmed doesn't, as far as I can see, say anything to explain why it is 'catastrophic' to have dilemmas in finite cases but acceptable in infinite cases, and I can't see any reason it would be.

The second argument I'm interested in turns on the assumed impossibility of dilemmas. Assume that some particular decision problem is salient, and the critic launches this argument.

1.  Causal ratificationism says every option other than X is non-ideal.
2.  Option X is non-ideal.
3.  Therefore, causal ratificationism says that something non-ideal, namely X, is ideal.

This argument is invalid. If there are dilemmas, then causal ratificationism can agree that X is non-ideal, even if all the other options are non-ideal.

That second argument is one we see frequently in the literature. The third one is not stated as frequently, but is I think sometimes implicit, and in any case is interesting.

1.  If a theory says all options in a particular problem are non-ideal, then it does not discriminate between the options.
2.  In some problems where causal ratificationism says there is a dilemma, there are reasons to discriminate between the options.
3.  So causal ratificationism is false, since it does not make distinctions that the best theory should make.

This argument is in a sense right, but not in a way that upsets the primary thrust of this book. What this kind of reasoning shows (and we'll see several concrete instances of it as the chapter proceeds) is that causal ratificationism is incomplete. This shouldn't be surprising. It's a theory of ideal decision, and a complete theory should be supplemented by a theory of non-ideal decision. The final goal of this chapter will be to point in the direction of such a non-ideal theory, and to discuss the relationship between ideal and non-ideal theory.

## Four Puzzle Cases {#fourpuzzles}

### Outguess the Demon

Table \@ref(tab:first-dilemma) is a simple case that can be used to show that causal ratificationism implies the existence of dilemmas, cases where there is no action that is ideally rational.

```{r, first-dilemma, cache=TRUE}
first_dilemma <- tribble(
	   ~"", ~PU, ~PD,
	   "U", "0", "2",
	   "D", "1", "0"
	)
gameformat(first_dilemma, "A simple example of a dilemma.")
```

Assume first, for simplicity, that Chooser can't play any mixed strategy. Chooser simply has to play U or D, and is arbitrarily confident that Demon will guess their choice correctly. Then Chooser will regret their choice whatever they do. But ideally rational choices are not regretted as soon as they are chosen. So no choice is ideally rational.

The case I've used here is a somewhat asymmetric version of the *Death in Damascus* case from @GibbardHarper1978. A slightly more common asymmetric version, as in @Richter1984, uses payouts like those in \@ref(tab:second-dilemma). In that case, Chooser prefers U over D both when Demon predicts correctly and when they predict incorrectly.

```{r, second-dilemma, cache=TRUE}
second_dilemma <- tribble(
	   ~"", ~PU, ~PD,
	   "U", "1", "3",
	   "D", "2", "0"
	)
gameformat(second_dilemma, "A version of asymmetric Death in Damascus.")
```

The cases seem to raise the same philosophical issues as far as I can see, and table \@ref(tab:first-dilemma) is a little simpler, so I'll focus on it.

There is, I gather, a widespread intuition amongst philosophers that ideal decision theory should treat U and D asymmetrically in this case. I don't really share that intuition; they both look like unhappy choices to me. And it isn't shared by orthodox game theory. When table \@ref(tab:first-dilemma) is turned into a game, it becomes \@ref(tab:first-dilemma-game).

```{r, first-dilemma-game, cache=TRUE}
first_dilemma_game <- tribble(
	   ~"", ~PU, ~PD,
	   "U", "$0, 1$", "$2, 0$",
	   "D", "$1, 0$", "$0, 1$"
	)
gameformat(first_dilemma_game, "The game version of the simple dilemma.")
```

That game has a unique Nash equilibrium. In equilibrium, Row, or Chooser, plays a 50/50 mix of U and D. The asymmetry in the payouts is reflected in an asymmetry in Column, or Demon's, equilibrium strategy. Their equilibrium strategy is to play $PU$ with probability $\frac{2}{3}$, and $PD$ with probability $\frac{1}{3}$. And that seems intuitively right to me; Chooser should treat the options here symmetrically, and if anyone should treat them asymmetrically, it is Demon.

Thinking about the equilibrium shows us how to drop the assumption that Chooser can't play mixed strategies, while keeping the conclusion that there are dilemmas. In \@ref(tab:first-dilemma) the standard setup is that Demon is an arbitrarily good predictor who is an expected utility maximiser. That settles what Demon will do in all but one case: when Chooser plays a 50/50 mix of U and D. Unfortunately, that's exactly what Chooser will do in equilibrium. So it's an important oversight. To fill it in, let's stipulate that Demon will also play a 50/50 mix of $PU$ and $PD$ if each of them have the same expected utility. That's enough to reinstate the dilemma.

Every strategy for Chooser is a mixture[^demons-and-decisions-7] where U is played with probability $p$, and D is played with probability $1-p$. If $p < 0.5$, then Chooser thinks Demon will play $PD$, so they should definitely play U, so they regret any strategy other than U for sure. And that means they regret playing this mixture where $p < 0.5$. If $p > 0.5$, then Chooser thinks Demon will play $PU$, so they should definitely play D, so they regret any strategy other than D for sure. And that means they regret playing this mixture where $p > 0.5$. And if $p = 0.5$, then Chooser thinks Demon will play a 50/50 mixture of $PU$ and $PD$, in which case they think the right strategy is to play U for sure. (This is where the asymmetry in the payoffs matters.) And so they regret the mixed strategy where $p = 0.5$. Whichever strategy, mixed or pure, that Chooser adopts, they regret it. So no strategy, mixed or pure, is ideally rational.

[^demons-and-decisions-7]: I'm assuming here that the pure strategies U and D are improper mixtures, where Chooser plays one of them with probability 1, and the other with probability 0. This assumption isn't needed for the proof; but it simplifies the presentation.

It's easy to think that if mixed strategies are allowed, then the fact that finite games have equilibria means they must allow strategies that won't be regretted. The problem with that reasoning is that it assumes more about Demon's dispositions than is known. It isn't stipulated that Demon is perfectly rational, or that they are an equilibrium seeker. It is just stipulated that they are an excellent predictor and a utility maximiser. And that isn't enough to rule out dilemmas, as this example shows.

I'll have much more to say about mixed strategies as this chapter progresses. But I'll leave them here, and turn to dilemmas that are not unique to causal ratificationism.

### Open Ended Good {#openendedgood}

Here is a very familiar example of a case where every theory says there is no ideally rational choice.

> **Open Ended Good**\
> Chooser must pick a real in $[0, 1)$. The higher the number they pick, the greater their reward will be.

For any $x \in [0, 1)$, if Chooser picks $x$, they could have instead picked $\frac{1+x}{2}$, which would have been better. So any pick they make is bad, either after they make it (as matters for causal ratificationism), or before they make it (as matters for most other theories). So it is a dilemma.

It is a dilemma that involves there being infinitely many choices. But it's hard to see why that should make a philosophical difference. I'll return to this point in section \@ref(whyallow), but for now simply note that any argument that dilemmas are impossible on theoretical grounds must say that **Open Ended Good** is somehow impossible. And it's hard to see why it would be.

### Heaven {#heaven}

One reason someone may worry about **Open Ended Good** is that it involves discontinuous payouts. The limit payout, as $x$ tends to 1, of choosing $x$ is 1. But the payout of choosing 1 is undefined. If we allow unlimited payouts, this discontinuity goes away. Unlimited payouts are sometimes thought to be of dubious coherence due to paradoxes like St. Petersburg, and I'm somewhat sympathetic to this line of thought. But they can be motivated, even in paradoxical situations.

> **Heaven**\
> Chooser has died, and gone to face the last judgment. God looks over Chooser's varied and active life, and is struggling to make a final call. He eventually says, "Look there is so much bad here I can't just let you into Heaven. But there is so much good that I can't just send you away either. So here's what I'll do. You'll stay a while here, then off to the other place. I've been struggling with thinking about how long you should stay here." And at this point, Chooser is hoping God picks a very large number. Chooser has had a bit of a look around while God was deciding. This is strictly speaking against the rules, but as we mentioned, Chooser had a varied and active life. Chooser has realised that (a) days in Heaven are good, and the goodness of them does not diminish over time, (b) the other place is considerably less good, and (c) the afterlife is infinite in duration. After a pause, God speaks again. "You know what, you decide. Pick a number, any number, any natural number that is, and you'll spend that many days here, then off to the other place." What should Chooser do?

Again, Chooser faces a dilemma. If Chooser picks $n$, then it would have been considerably better to pick $2n$, or $n!$, or $(n!)!$. Whatever choice Chooser makes, will be regretted instantly.

It is worth thinking through this case a bit, to get a feel for what dilemmas are like from the inside. I think there is a common view that decision theory shouldn't allow dilemmas because it should be practical, that it should offer advice. And saying in cases like table \@ref(tab:first-dilemma) that whatever one does, one will regret it, isn't exactly advice. But in **Heaven**, that's obviously the right thing to say. Or, at least, it's obviously part of the right thing to say. And both of these insights, that everything is regrettable is both correct and incomplete, will be important in what follows.

### The Salesman

The last two examples are dilemmas on every theory of decision. **Outguess the Demon** is a dilemma according to causal ratificationism, but not according to most other theories. I'll end this section with a case that is harder to classify. It is **Salesman**, the problem from section \@ref(deccertainty). The aim is to find as short a path as possible through these cities, assuming that it is possible to travel between any two cities in a straight line.

```{r salesman-points-reprise, fig.cap="The 257 cities that must be visited in the Salesman problem (reprise)."}
tour_map_points_only
```

For an ideal agent, this isn't a dilemma; there is a shortest path. But for normal people, it is a dilemma. At least, it satisfies the definition of a dilemma that I've given. And thinking through the case helps motivate that definition, and indeed helps clarify dilemmas in general.

Just about the least thoughtful path possible through the 257 cities orders the cities alphabetically. The resulting tour is shown in figure \@ref(fig:alpha-tour).

```{r drawmap-definition, cache=FALSE}
drawmap <- function(tour_line){
  paths <- tribble(
    ~step, ~property, ~rowid, ~long, ~lat
  )
  
  for (i in 1:nrow(our_gps)){
    x <- tour_line[i]
    first_city <- our_gps %>% slice(x)
    next_city <- our_gps %>% slice(x %% 31)
    paths <- paths %>%
      add_row(step = i, property = "from", rowid = first_city$rowid[1], long = first_city$long[1], lat = first_city$lat[1])# %>%
    #    add_row(step = i, property = "to", rowid = next_city$rowid[1], long = next_city$long[1], lat = next_city$lat[1])
  }
  
  x <- tour_line[1]
  
  paths <- paths %>% add_row(step = 24, property = "from", rowid = our_gps$rowid[x], long = our_gps$long[x], lat = our_gps$lat[x])
  
  
  state_map_data <- map_data("state") %>%
    #  filter(subregion != "north" | is.na(subregion)) %>%
    filter(region %in% long_states) 
  
  tour_map <- ggplot(state_map_data, aes(long, lat, group = group)) +
    geom_polygon(fill = "white", colour = "grey90") + 
    geom_point(data = our_gps %>% select(long, lat), aes(x = long, y = lat), size = 0.25, inherit.aes = FALSE) +
    geom_path(data = paths %>% select(long, lat), aes(x = long, y = lat), inherit.aes = FALSE, colour = "grey30", alpha = 0.5 ) + 
    coord_quickmap() +
    labs(x = paste0("Tour length: ", tour_length(tour_line), " miles.")) +
    theme(axis.title.x = element_text())
  #tour_length(tour_line)
  tour_map
  
}
```

```{r alpha-tour, cache=TRUE, fig.cap="A solution to the salesman problem that orders the cities alphabetically."}
tour_line <- solve_TSP(as.TSP(city_matrix), method="identity")
drawmap(tour_line)
```

It's only slightly more thoughtful to order the cities from west-to-east, as in figure \@ref(fig:longit-tour), but it does reduce the length by nearly two-thirds.

```{r longit-tour, cache=TRUE, fig.cap="A solution to the salesman problem that orders the cities by longtitude."}
tour_line <- TOUR(arrange(our_gps, long)$rowid, tsp = as.TSP(city_matrix))
drawmap(tour_line)
```

Let's try something slightly more thoughtful. Start in an arbitrary city, I'll use New York, and then at each step go to the nearest city that isn't yet on the path. The result looks tour is shown in \@ref(fig:newyork-tour).

```{r newyork-tour, cache=TRUE, fig.cap="A solution to the salesman problem that chooses the nearest remaining city."}
bad_path <- c(153)

for (i in 1:256){
  latest_city <- bad_path[i]

    lengths <- our_cities %>% 
    select(all_of(latest_city)) %>% 
    rowid_to_column() %>% 
    filter(!rowid %in% bad_path)
    
    lengths <- lengths %>% 
    arrange(across(names(lengths)[2]))
  
bad_path <- c(bad_path, lengths$rowid[1])
}
tour_line <- TOUR(bad_path, tsp = as.TSP(city_matrix))
drawmap(tour_line)
```

It's better, but it still looks not so great in a few respects. For one thing, it crosses over itself too many times. For another, those very long lines seem like mistakes. And they are somewhat inevitable consequences of this approach. Always choosing the nearest city will mean sometimes the path leaves a region without clearing it, and has to come back. So in this map there is a single step from Wyoming to New Jersey, as the map goes back to clean up the cities near the start that were missed.

Here's a much better idea, in two stages. For both stages I've used the implementation of these ideas in the TSP package by Michael Hashler and Kurt Hornik, and what follows is from their description [-@HashlerHornik2007].

The first stage uses the *farthest-insertion* method. The idea is to build the path in stages. At each stage, the algorithm identifies the city that is farthest from the existing path. It then inserts that city into the path at the spot where it will make the least addition to the path length. These kind of insertion algorithms are common. What makes the farthest-insertion algorithm work well is that it forces the path to start with something like a loop around the edges of the map, and this is a generally good approach.

The second stage uses the *two-optimization* method. This takes a completed path as input, and tries to improve it by seeing if the path would be shortened by flipping adjacent points. It does this repeatedly until it finds a local minimum. One way to turn this into an algorithm is to start with a random path. But what I did here was start with the path that a particular run of the farthest-insertion method generated. The farthest-insertion algorithm needs a start city, or it will choose one randomly. So for reproducibility purposes, I used New York again. And I set the *seed* for the random number generator in R to 1. And the result was the elegant path shown in figure \@ref(fig:two-stage-tour).

```{r two-stage-tour, cache=TRUE, fig.cap="A solution to the salesman problem that uses two algorithms."}
set.seed(1)
tour_line <- solve_TSP(as.TSP(city_matrix), method="farthest_insertion", start=153)
tour_line <- solve_TSP(as.TSP(city_matrix), method="two_opt", tour = tour_line)
drawmap(tour_line)
```

That map has a lot of virtues. There aren't any obvious missteps on the map, where it's easy to see just by looking that it could be shortened. The theory behind it makes sense. It was generated very quickly. It took my computer more time to draw the map than to find the path. And it is a very short path compared to the other maps we've seen.

But it isn't optimal. I'm not sure what is optimal, but the shortest one I found after trying a few methods is shown in figure \@ref(fig:two-stage-optimal).

```{r two-stage-optimal, cache=TRUE, fig.cap="The best solution I found to the salesman problem."}
# Converting the concorde generated tour into our form
my_list <- as.integer(unlist(strsplit("0,84,59,240,13,198,121,58,102,89,170,20,212,135,231,127,136,107,147,17,151,97,24,142,162,228,87,229,194,206,116,138,244,158,61,108,207,44,54,11,132,9,55,143,26,86,103,47,117,7,96,216,46,251,95,184,69,179,250,174,153,183,242,15,99,119,110,181,248,4,249,164,10,234,71,148,109,152,161,246,222,42,33,150,137,149,100,219,252,175,78,34,30,38,123,130,134,57,173,171,12,16,144,36,32,168,235,2,208,239,25,226,186,35,74,254,167,223,245,39,1,51,256,45,8,56,221,60,98,50,124,129,31,146,159,77,230,118,104,145,83,125,232,6,65,81,19,189,120,106,18,92,112,215,90,49,115,178,139,210,94,133,187,163,28,43,238,62,218,192,53,220,111,82,237,156,73,247,196,233,113,114,191,126,157,213,214,64,243,41,105,67,185,70,225,68,193,140,190,79,141,27,166,180,211,23,93,101,37,169,155,197,176,29,217,241,253,21,209,227,172,195,75,76,22,154,201,204,202,224,188,182,40,85,14,203,128,160,199,200,255,122,80,165,236,72,205,3,88,91,48,63,52,177,66,5,131",",")))
my_list_adj <- my_list + 1
tour_line <- TOUR(my_list_adj, tsp = as.TSP(city_matrix))
drawmap(tour_line)
```

That was the best I could find with the tools I had, and I'm not sure it is even optimal. It would actually be helpful for the argument I'm about to make if it isn't optimal, and the shortest path is shorter again. But let's assume that it, or something like it, is. Because what I want us to focus on is the philosophical status of the tour in figure \@ref(fig:two-stage-tour). Getting clear on that will be the beginning of a solution to the puzzles about dilemmas.

## Common Features of the Puzzles {#commonfeatures}

The four puzzles from section \@ref(fourpuzzles) have a lot of features in common. This is relevant both because these commonalities support the claim I've made that **Salesman** is a dilemma, and because they make it more reasonable to think that we can learn from one example what's true in other examples.

### Multiple Standards

Philosophers have high standards. In this field, as in many others, this is more of a curse than a blessing. It means that the question *Is this ideal?* is often not just the first question, but the last question. That approach leads to one losing a lot of information. A lot of things that are not ideal are interesting, useful, and good, and if one can't tell them apart from other non-ideal things that are boring, useless, and bad, one is missing out on a lot.

This is particularly clear in **Salesman**. The farthest-insertion algorithm is not ideal, but it is much better than most approaches, especially in situations where the cost of computation is high relative to the cost of travel. By that I don't just mean that it produces shorter routes, though it does do that. I mean that it is qualitatively, not just quantitatively, better than some alternatives. It is a very good algorithm. And what I want us to focus on for a little bit is this idea of being *very good*.

Being very good is an evaluative standard. It is qualitative in the sense I meant in the previous paragraph. Saying that a strategy is very good is to say that it is more rational strategies that are bad, or poor, or mediocre, or fine. This isn't just to say that it produces better outcomes than strategies that are bad, poor, mediocre, or fine. It is easy to see this in **Open Ended Good**. Answering 0.2 produces a better outcome than answering 0.1. But both of them are absurdly bad answers. They are qualitatively alike, even though one of them is quantitatively better. (Indeed, twice as good.) There are only finitely many standards like *very good* or *absurdly bad*, but there are continuum many answers to **Open Ended Good** that are strictly ordered from quantitatively worst to better.

All this matters because it blocks an inference that is otherwise tempting. If one makes no distinction between non-ideal statuses, then one is left saying that decision theory has nothing to say in the case of dilemmas. But that's not true; it can still say that this or that decision is more rational or less rational.

### Stake Sensitivity

If a solution to a decision problem is ideal, it stays ideal if the stakes of the problem change. But that isn't true of the other statuses I described in the previous subsection, like being *absurdly bad* or *very good*. It's easiest to see this with **Open Ended Good**. Here are two versions of the problem, with the details filled in.

> **Open Ended Good (Dollars)**\
> Chooser must pick a real in $[0, 1)$. They will get as many dollars as they pick. (Chooser lives in a world somewhat like ours, except that bank accounts can have any real valued number of dollars in them.)

> **Open Ended Good (Lives)**\
> Chooser must pick a real in $[0, 1)$. From now on, every human born will have a new disease, that kills some people at random on their eighteenth birthday. The real Chooser picks will determine what proportion of people survive. If they choose 0.7, for example, then 30% of people will die from the disease.

In **Dollars**, selecting 0.999 is fine, even fairly good. In **Lives**, selecting 0.999 is awfully bad, actually fairly immoral. The same thing goes for the other problems in this chapter. (Except for **Heaven**, where the details are filled in so much that there is not the potential for more variation.) In **Salesman**, first consider a version where each extra mile will cost Chooser a penny, and Chooser has to work out the paths by hand. The path in \@ref(fig:newyork-tour) is not bad in that situation. Chooser could save \$50 by coming up with one of the other paths, but it would take hours and hours of work to come up with it. Now consider a version where they have fancy software and hardware to throw at the problem, but a person will die for each mile they take beyond the shortest path. Then even \@ref(fig:two-stage-tour), which I'd said was pretty good, would be awful. Chooser should instead learn how to use one of the algorithms that find the exact solution, and use it.

### Resource Sensitivity {#resource}

This may have been implicit in what I said above, but dilemmas are sensitive not just to how much is at stake, but what resources the chooser has for resolving them. This is clear in **Salesman**. The path in \@ref(fig:newyork-tour) is reasonable if Chooser is solving the problem with pen and paper, and unreasonable if they are solving it with modern technology. But it also applies in **Open-Ended Good**. Assume Chooser has so little mathematical knowledge that they can only answer by saying *zero point nine nine nine etc*. If they are answering verbally, and the stakes are moderate, it might be reasonable to end after eight to ten repetitions. But if they have a computer, and can answer by writing a number on screen, the reasonably thing to do is to hold down the nine key for a while and have it repeat.

This puts a real constraint on what a non-ideal theory must look like. Existing theories of decision, at least in the philosophical literature, are not resource-sensitive. So they can't be good theories of non-ideal decision, at least as they stand.

## No Ideals {#noideals}

This section argues for something that has been implicit in everything I've said in this chapter: *dualism* about ideal and non-ideal decision theory. By dualism, I mean the view that neither ideal nor non-ideal decision theory are reducible to the other. There are a couple of reductions that are, prima facie, rather attractive. One takes ideal decision theory to be primary, and says that non-ideal decision theory is just ideal decision theory with different costs and benefits of options, and perhaps different available options. Another takes non-ideal decision theory to be primary, and says that ideal decision theory is the special case of non-ideal decision theory that one gets when conditions are, as a matter of fact, ideal. I reject both views; I think ideal and non-ideal decision theory are distinct subject matters. And this section argues for the division.

### Outguessing Again {#outguess-reprise}

The decision problem in table \@ref(tab:third-dilemma) is just table \@ref(tab:second-dilemma) with the labels changed.

```{r, third-dilemma, cache=TRUE}
third_dilemma <- tribble(
	   ~"", ~PU, ~PD,
	   "U", "0", "2",
	   "D", "3", "1"
	)
gameformat(third_dilemma, "Another version of asymmetric Death in Damascus.")
```

I think the following things are true about this game.

1.  Ideal theory says that it is a dilemma, neither choice is ideal.
2.  Non-ideal theory says that it is better (in most situations) to choose D rather than U.

In this subsection I'll argue for 1, and in the rest of this section I'll argue for the consistency of 1 and 2. In the next section I'll argue directly for 2, though I suspect that part of the argument won't meet much resistance. The real point of the next section will be to say something substantive about the nature of non-ideal decision theory; the defense of 2 will fall out.

Given 1 and 2, the argument for dualism is straightforward. Any plausible reduction of ideal to non-ideal decision theory will say that 1 and 2 are inconsistent. If they are both true, then there is no possible reduction. And hence dualism is true.

The argument for 1 is primarily an instance of the ABC argument from chapter \@ref(decisive). Consider a version of the ABC game with the following characteristics:

-   Demon gets 0 for a false prediction.
-   Demon gets 1 for correctly predicting U, or for choosing A.
-   Demon gets 2 for correctly predicting D.
-   If Demon doesn't choose A, Chooser's payouts are as in table \@ref(tab:third-dilemma).
-   If Demon chooses A, Chooser gets 1.5.
-   If Demon predicts U, Demon chooses A with probability $\frac{8}{9}$, and B with probability $\frac{1}{9}$.

Before Demon makes a choice, the strategic form of the game facing Chooser looks like table \@ref(tab:third-dilemma-strategic).

```{r, third-dilemma-strategic, cache=TRUE}
third_dilemma_strategic <- tribble(
	   ~"", ~PU, ~PD,
	   "U", "4/3", "2",
	   "D", "5/3", "1"
	)
gameformat(third_dilemma_strategic, "Chooser's options before Demon makes a move.")
```

All the arguments in chapter \@ref(decisive) carry over, so according to ideal theory, a choice is permissible in table \@ref(tab:third-dilemma-strategic) iff it is permissible in table \@ref(tab:third-dilemma). But any reason for choosing D in table \@ref(tab:third-dilemma) works equally well as a reason for choosing U in table \@ref(tab:third-dilemma-strategic). So the view that D is the uniquely rational choice in table \@ref(tab:third-dilemma) is dynamically incoherent, and should be rejected.

This doesn't quite seal the argument for it being a dilemma. For all this shows, it could be that both U and D are ideally permissible in table \@ref(tab:third-dilemma). The argument here relies on the instability of both U and D. Each of those choices is such that, once made, the chooser is unhappy with it. Intuitively, an ideal choice should not be like that. It is particularly hard to see how U in table \@ref(tab:third-dilemma) could be ideal; by any natural measure it is leaving money on the table. Since it is not ideally permissible, and the ABC argument shows that either both options are ideally permissible or neither is, it follows that neither option is ideally permissible. The case is, from the perspective of ideal decision theory, a dilemma.

### Foot's Distinction {#foot}

But surely what I just said is a contradiction. Here is a very simple argument against the conclusions of the last subsection.

1.  If there are two options, and one is better than the other, the first one is ideal.
2.  In table \@ref(tab:third-dilemma), there are two options, and D is clearly better than U.
3.  So, in table \@ref(tab:third-dilemma), D is ideal.

I think the second premise is true, and the conclusion is false. Since the argument is valid, I'm committed to denying the first premise. But it looks like an analytic truth.

To see what it wrong with premise 1, we need to take a detour through work on moral dilemmas, and in particular pay attention to a distinction drawn by Philippa @Foot1983. She distinguishes *type 1* and *type 2* ought statements. It is easiest to understand the distinction with an example she uses. A person has made two incompatible promises and cannot keep both. But it would be an inconvenience to break one, and a disaster to break the other. Ought the person keep the first promise, the one it would be an inconvenience to break? There is, Foot argues, a sense in which the answer is *yes*, and a sense in which the answer is *no*. And that suggests that there are two notions of *ought*. The *type 1* notion is the one in which the person ought keep the promise; one ought keep one's promises. The *type 2* notion is the one in which the person ought not keep it; they have to break a promise, and it would be better to break this one. The names aren't the most evocative, but I think Foot is right that there are simply two notions here, and we shouldn't aim to say which of them is what *ought* really means.

I say the same thing about table \@ref(tab:third-dilemma). In neither sense ought Chooser choose U. But ought they choose D? No, in the type 1 sense; yes, in the type 2 sense. They must choose D or U, and ought not choose U. That implies that they ought choose D, but only in the type 2 sense of ought.

The key thing to make all of what I've said so far consistent is that ideal decision theory is a theory of what one ought do, in Foot's type 1 sense. There is independent evidence that this is the case. What one ought do in the type 2 sense is sensitive to stakes, and computation costs. Non-ideal decision theory, as I discussed above, is sensitive to stakes, and to computation costs. But ideal decision theory is not sensitive to these things. All of the cases I've discussed are ones where we have to distinguish between what's best and what's ideal, so they are all cases where Foot's distinction is important.

I'm not the first to note that this kind of distinction could matter to decision theory. Paul @Weirich1985 says that it is absurd to say that there are dilemmas in decision theory, but that ideal decision theory is impossible in cases like table \@ref(tab:third-dilemma). That requires making Foot's distinction. If a dilemma just means a situation where no solution is ideal, then Weirich has simply contradicted himself. That would be an absurd reading; a better reading is that he's distinguishing between what is ideal (Foot's type 1), and what should be done all things considered (Foot's type 2). Now I'm not as sure that dilemmas in the second sense are impossible; after all, in **Heaven** there might be nothing that one ought do in the type 2 sense. But I think the best way to read what Weirich says is as making the same kind of distinction as Foot makes, and which I'm relying on here.

### The Sure Thing Principle in Non-Ideal Decisions {#salesurething}

In this section I'm going to go over a somewhat complicated variant of **Salesman** which has striking consequences for how one should think about non-ideal decision making. The basic idea for this example is due to Nick Smith (in conversation), but he shouldn't be held responsible for the details of how I'm setting it out.

Chooser is shown a list of one thousand lists of ten cities each. Each of the ten cities was chosen at random from the 257 cities (independently over each of the thousand choices), with the exception that South Bend, Indiana, and Kalamazoo, Michigan, are excluded from the list. One of these thousand lists will be chosen at random. Chooser is asked the following question.

-   In the shortest path through the 247 remaining cities (once the ten listed cities are excluded), is there a direct path from South Bend, Indiana, to Kalamazoo, Michigan?

Now this is a hard question twice over. Chooser doesn't even know which are the 247 cities, although Chooser knows that the set of 247 is one of a thousand possible sets. And Chooser doesn't know what the shortest path through each of these sets of 247 cities is, because these problems are hard. Happily, Chooser has three possible answers to this question: *Yes*, *No*, or *Pass*. If Chooser says *Yes* or *No*, they get \$100 if they are correct, and nothing otherwise. If they say *Pass*, they get \$80.

Three more details about Chooser's situation, then we'll get onto the analysis. First, Chooser has a decent computer, and knows a bit about how to use it to solve problems like **Salesman**. The time and effort it costs Chooser to solve a particular instance of a salesman problem with about 250 cities is worth about \$10. So if Chooser were told which 247 cities were to be mapped out, Chooser would use the computer, figure it out, and get the \$100. But it's not worth doing this a thousand times over for \$20. Second, from playing around with these problems, Chooser's prior probability that the shortest path for one of these sets of 247 cities would include a link direct from South Bend to Kalamazoo is about 0.5. There are very natural maps that include such a link, and very natural maps that don't. Indeed, there are plenty of natural maps which have the cities on very different parts of the path. Even when the shortest path includes a direct link, there will be a very short path that does not. Excluding a city from a very different part of the map might make all the difference in whether there should be a direct link. Third, although there is no way Chooser could know this, in fact for any one of the thousand sets of ten cities on the list, the answer is *Yes*. This is a remarkable coincidence, and it's one Chooser doesn't know, and couldn't possibly guess.

Given all this, there are two natural conclusions to draw. In the actual situation Chooser is in, they should say *Pass*. They'll get \$80. If they said *Yes* or *No*, they would be guessing, and the expected return of the guess is about \$50. They could work it out, but it's not worth the hassle. But, if Chooser were shown which ten cities had been chosen, they would work out what the shortest path was, and say the correct answer *Yes*.

Two philosophical conclusions follow from those points.

First, the Sure Thing Principle doesn't apply in cases of non-ideal decision theory. Whatever list from the one thousand lists was chosen, Chooser would say *Yes*, and rationally so. But before the choice is made, Chooser says *Pass*, and rationally so. I've made heavy use of the Sure Thing Principle for ideal decision theory, and I think it is a key part of ideal decision theory. But it is false when applied to non-ideal decision theory.

Second, sometimes the best thing to do in a non-ideal case is something Chooser knows is not what ideal decision theory would recommend.

Both of these points support Dualism. If ideal and non-ideal decision theory have different logics, which is what follows from one but not the other obeying Sure Thing, then it is unlikely that one can be reduced to the other. And the most natural reductions involve the idea that one should not do, even in non-ideal cases, what is known not to be ideal.

And this also explains why the ABC argument, which is so central to the discussion here of ideal decision theory, is irrelevant to non-ideal decision theory. Without Sure Thing, a key premise of the ABC argument is not supported. And so it is reasonable in non-ideal decision theory to make a distinction between cases that are, from the perspective of ideal decision theory, alike.

## Evaluating the Non-Ideal

So far I've argued that it is consistent to say that table ref is a dilemma, and that it is better to choose D than U. I now turn to the question of whether D is in fact better, and, if it is, why it is.

The short version will be that non-ideal strategies should be evaluated not in terms of how well they do on an occasion, or even in terms of how well they respond to the available evidence on an occasion, but in terms of how well they do generally. But what is it for a strategy to do well *generally*? That turns out to be a hard question, and not one that I'll make much progress on settling here. The aim of this section is to set up this question, and point towards what a resolution of it might look like.

### Salesman, One Last Time

As with most things about dilemmas, and non-ideal decision theory, **Salesman** turns out to be a useful guide. So let's think about one last particular case of it. Fargle and Nargle are solving the version of Salesman in figure

```{r west-coast-tour, fig.cap="The 21 cities that must be visited in the west coast salesman problem."}
theme_set(theme_map())

all_states <- map_data("state") %>% 
  group_by(region) %>% 
  tally() %>% 
  select(state = region)

all_states$code <- c("AL", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA",
                     "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", 
                     "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", 
                     "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", 
                     "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

used_states <- c(36, 4, 27)

long_states <- all_states$state[used_states]
short_states <- all_states$code[used_states]

data("USCA312")
data("USCA312_GPS")

cities <- as_tibble(as.matrix(USCA312))

city_numbers <- tibble(
  id = 1:312,
  thecities = colnames(cities)
) %>% 
  mutate(used_city = case_when(str_sub(thecities, -2) %in% short_states  ~ 1,
                               TRUE ~ 0))

the_city_numbers <- filter(city_numbers, used_city == 1)$id


our_cities <- cities %>% 
  select(all_of(the_city_numbers)) %>% 
  slice(the_city_numbers)

our_gps <- USCA312_GPS %>% 
  slice(the_city_numbers) %>% 
  rowid_to_column()

city_matrix <- as.matrix(our_cities)

rownames(city_matrix) <- filter(city_numbers, used_city == 1)$thecities

length_tib <- tibble(st = 1:length(the_city_numbers)) 

length_tib <- length_tib %>% 
  rowwise() %>% 
  mutate(farth = tour_length(solve_TSP(as.TSP(city_matrix), method="farthest_insertion", start = st))) %>% 
  mutate(near = tour_length(solve_TSP(as.TSP(city_matrix), method="nearest_insertion", start = st))) %>% 
  ungroup() %>% 
  mutate(diff = near - farth) %>% 
  arrange(diff)

best_diff <- min(length_tib$near) - max(length_tib$farth)

near_tour <- solve_TSP(as.TSP(city_matrix), method="nearest_insertion", start = 7)
far_tour <- solve_TSP(as.TSP(city_matrix), method="farthest_insertion", start = 19)

tour_line <- far_tour

# Turn tour to map path
paths <- tribble(
  ~step, ~property, ~rowid, ~long, ~lat
)

for (i in 1:nrow(our_gps)){
  x <- tour_line[i]
  first_city <- our_gps %>% slice(x)
  next_city <- our_gps %>% slice(x %% 31)
  paths <- paths %>%
    add_row(step = i, property = "from", rowid = first_city$rowid[1], long = first_city$long[1], lat = first_city$lat[1])# %>%
  #    add_row(step = i, property = "to", rowid = next_city$rowid[1], long = next_city$long[1], lat = next_city$lat[1])
}

x <- tour_line[1]

paths <- paths %>% add_row(step = 24, property = "from", rowid = our_gps$rowid[x], long = our_gps$long[x], lat = our_gps$lat[x])


state_map_data <- map_data("state") %>%
  #  filter(subregion != "north" | is.na(subregion)) %>%
  filter(region %in% long_states)

tour_map <- ggplot(state_map_data, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "grey90") +
  geom_point(data = our_gps %>% select(long, lat), aes(x = long, y = lat), size = 0.25, inherit.aes = FALSE) +
#  geom_path(data = paths %>% select(long, lat), aes(x = long, y = lat), inherit.aes = FALSE, colour = "grey30", alpha = 0.5 ) +
  coord_quickmap() +
#  labs(x = paste0("Tour length: ", tour_length(tour_line), " miles.")) +
  theme(axis.title.x = element_text())
#tour_length(tour_line)
tour_map
```

This is just like the version that I've used many times so far, constrained to California, Nevada, and Oregon. Both of them know that *insertion* algorithms are very good ways to quickly get a short path through the points. And in both cases, it is reasonable, given the stakes involved in the problem and the computational resources available, to use a single run through an insertion algorithm to find a solution. So that's what they will do.

But they use different insertion algorithms. Recall how insertion algorithms go. The full path is built by adding a city into the path at each step. To go from, say, fifteen cities to sixteen, the algorithm looks at the fifteen options for inserting the new city between each of the existing adjacent cities on the path, and chooses the shortest one. Then it looks to add the seventeenth city by inserting it between two paths on the existing path, and so on until all the cities are included. There are three kinds of choice points in running an insertion algorithm: which city to start with, how to choose the city to be inserted, and how to break ties. The last is usually done at random, and it introduces some noise into the process. The first is also done somewhat arbitrarily, although it turns out to make a big difference. But the second we can be more theoretical about.

Two natural versions of the insertion algorithm are the *farthest insertion* and the *nearest insertion*. The farthest insertion algorithm chooses at each stage the city that is farthest from the path, and looks at the way to insert it onto the path with minimal increase in length. The nearest insertion algorithm chooses at each stage the city that is closest from the path, and looks at the way to insert it onto the path with minimal increase in length. In the vast majority of cases, the farthest insertion algorithm does better. A lot of people find this counterintuitive, as I did when I first learned it. But the reason it works is that it forces the path to start with a giant loop around the edge of the map, and in general the shortest paths have the structure of giant loops with interior cities reached by small detours.

Fargle and Nargle both know this fact about insertion algorithms. Fargle reacts to this sensibly, by using a farthest insertion algorithm, and ending up with the path shown on the left of figure \@ref(fig:near-and-far). Nargle reacts less sensibly, by choosing a nearest insertion algorithm, and ending up with the path shown on the right of \@ref(fig:near-and-far).

```{r near-and-far, fig.cap="Two solutions to the west coast salesman problem.", fig.show="hold", out.width="50%"}
## A script to find a pair of paths where a typically worse algorithm does better on the particular use

require(tidyverse)
require(TSP)
require(maps)
set.seed(2)

theme_map <- function(base_size=9, base_family="") {
  require(grid)
  theme_bw(base_size=base_size, base_family=base_family) %+replace%
    theme(axis.line=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid=element_blank(),
          panel.spacing=unit(0, "lines"),
          plot.background=element_blank(),
          legend.justification = c(0,0),
          legend.position = c(0,0)
    )
}

theme_set(theme_map())

all_states <- map_data("state") %>% 
  group_by(region) %>% 
  tally() %>% 
  select(state = region)

all_states$code <- c("AL", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA",
                     "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", 
                     "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", 
                     "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", 
                     "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

used_states <- c(36, 4, 27)

long_states <- all_states$state[used_states]
short_states <- all_states$code[used_states]

data("USCA312")
data("USCA312_GPS")

cities <- as_tibble(as.matrix(USCA312))

city_numbers <- tibble(
  id = 1:312,
  thecities = colnames(cities)
) %>% 
  mutate(used_city = case_when(str_sub(thecities, -2) %in% short_states  ~ 1,
                               TRUE ~ 0))

the_city_numbers <- filter(city_numbers, used_city == 1)$id


our_cities <- cities %>% 
  select(all_of(the_city_numbers)) %>% 
  slice(the_city_numbers)

our_gps <- USCA312_GPS %>% 
  slice(the_city_numbers) %>% 
  rowid_to_column()

city_matrix <- as.matrix(our_cities)

rownames(city_matrix) <- filter(city_numbers, used_city == 1)$thecities

length_tib <- tibble(st = 1:length(the_city_numbers)) 

length_tib <- length_tib %>% 
  rowwise() %>% 
  mutate(farth = tour_length(solve_TSP(as.TSP(city_matrix), method="farthest_insertion", start = st))) %>% 
  mutate(near = tour_length(solve_TSP(as.TSP(city_matrix), method="nearest_insertion", start = st))) %>% 
  ungroup() %>% 
  mutate(diff = near - farth) %>% 
  arrange(diff)

best_diff <- min(length_tib$near) - max(length_tib$farth)

near_tour <- solve_TSP(as.TSP(city_matrix), method="nearest_insertion", start = 7)
far_tour <- solve_TSP(as.TSP(city_matrix), method="farthest_insertion", start = 19)

tour_line <- far_tour

# Turn tour to map path
paths <- tribble(
  ~step, ~property, ~rowid, ~long, ~lat
)

for (i in 1:nrow(our_gps)){
  x <- tour_line[i]
  first_city <- our_gps %>% slice(x)
  next_city <- our_gps %>% slice(x %% 31)
  paths <- paths %>%
    add_row(step = i, property = "from", rowid = first_city$rowid[1], long = first_city$long[1], lat = first_city$lat[1])# %>%
  #    add_row(step = i, property = "to", rowid = next_city$rowid[1], long = next_city$long[1], lat = next_city$lat[1])
}

x <- tour_line[1]

paths <- paths %>% add_row(step = 24, property = "from", rowid = our_gps$rowid[x], long = our_gps$long[x], lat = our_gps$lat[x])


state_map_data <- map_data("state") %>%
  #  filter(subregion != "north" | is.na(subregion)) %>%
  filter(region %in% long_states)

tour_map <- ggplot(state_map_data, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "grey90") +
  geom_point(data = our_gps %>% select(long, lat), aes(x = long, y = lat), size = 0.25, inherit.aes = FALSE) +
  geom_path(data = paths %>% select(long, lat), aes(x = long, y = lat), inherit.aes = FALSE, colour = "grey30", alpha = 0.5 ) +
  coord_quickmap() +
  labs(x = paste0("Tour length: ", tour_length(tour_line), " miles.")) +
  theme(axis.title.x = element_text(size=14))
#tour_length(tour_line)
tour_map

tour_line <- near_tour

# Turn tour to map path
paths <- tribble(
  ~step, ~property, ~rowid, ~long, ~lat
)

for (i in 1:nrow(our_gps)){
  x <- tour_line[i]
  first_city <- our_gps %>% slice(x)
  next_city <- our_gps %>% slice(x %% 31)
  paths <- paths %>%
    add_row(step = i, property = "from", rowid = first_city$rowid[1], long = first_city$long[1], lat = first_city$lat[1])# %>%
  #    add_row(step = i, property = "to", rowid = next_city$rowid[1], long = next_city$long[1], lat = next_city$lat[1])
}

x <- tour_line[1]

paths <- paths %>% add_row(step = 24, property = "from", rowid = our_gps$rowid[x], long = our_gps$long[x], lat = our_gps$lat[x])


state_map_data <- map_data("state") %>%
  #  filter(subregion != "north" | is.na(subregion)) %>%
  filter(region %in% long_states)

tour_map <- ggplot(state_map_data, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "grey90") +
  geom_point(data = our_gps %>% select(long, lat), aes(x = long, y = lat), size = 0.25, inherit.aes = FALSE) +
  geom_path(data = paths %>% select(long, lat), aes(x = long, y = lat), inherit.aes = FALSE, colour = "grey30", alpha = 0.5 ) +
  coord_quickmap() +
  labs(x = paste0("Tour length: ", tour_length(tour_line), " miles.")) +
  theme(axis.title.x = element_text(size=14))
#tour_length(tour_line)
tour_map

# Just do California and Nevada and Oregon
# Nearst starts in Las Vegas, NV
# Farthest starts in Reno, NV
# Learn how to do side by side pictures for this
# Have the players in the story pick these at random
# Reno, NV is a bad start location, kind of obviously so, but maybe player doesn't know that.
```

And as sometimes happens, Nargle does better. This is rare. (It took some looking to find a pair like this.) This does not show that Nargle was more rational; Fargle was more rational. Indeed, Fargle might have done as well as one could do given the available skills and resources. That's true even though there are many better routes that, in theory, could have been selected. For instance, if Fargle had selected literally any of the other twenty cities as their first city, they would have done better than Nargle. And this was a priori knowable when they set out.

**NOT COMPLETE**

## The Possibility Assumption in Philosophical Arguments {#badargument}

A bad argument form, and why it matters

### An Argument against EDT {#badargumentedt}

-   Open Ended Good
-   Of course, this is a bad argument

### A Recipe for Counterexamples {#recipe}

Once you see the idea behind the argument in subsection \@ref(badargumentedt), it's easy to see how to construct 'counterexamples' to any decision theory that allows for dilemmas. For instance, here is a general purpose recipe for constructing counterexamples to most forms of Causal Decision Theory, causal defensivism included.

Step One

:   Find some game where one player has demon-like payouts, and there is no pure strategy equilibrium for the other player. By a 'demon-like payout', I mean that player's payouts are all 1s and 0s, and you can sensibly interpret the 1s as situations where they correctly predicted the other player's choice.

Step Two

:   Translate that game into a demonic decision problem, taking care to stipulate that the human player is incapable of playing mixed strategies, or that something very bad will happen to them if they do.[^demons-and-decisions-8]

Step Three

:   Look at the pure strategies the player has, and identify the one that it would be least plausible to have one's theory recommend.

Step Four

:   Argue, correctly, that all strategies other than the one you've identified are irrational according to CDT.

Step Five

:   Infer that the remaining strategy is the one that CDT recommends.

Step Six

:   Point out that it is really unintuitive that CDT would recommend that strategy, declare that this is a clear counterexample to CDT, declare victory, etc.

[^demons-and-decisions-8]: I'm a bit perplexed as to why this stipulation is so widely accepted in the decision theory literature. But it usually seems to be accepted with minimal fuss. We'll return to this point in section \@ref(mixedavoidance).

Hopefully the discussion in section \@ref(badargument) will have made it clear that we can run this recipe against just about any theory, so it overgenerates. And not just that, we can identify the misstep - it's step five.

### An Instance of the Recipe

-   One of the Frustrator examples

### Betting Against the Demon {#againstdemon}

There is a slight variation on the recipe in an interesting example due to Arif @Ahmed2014 [sect 7.4.3]. Here the plan at step 6 is not to argue that CDT gets the wrong result, in fact Ahmed endorses the conclusion he thinks CDT ends up with, but to argue that CDT undermines its own principles. I'm going to lean a bit on the discussion of the example in the review of Ahmed's book by James @Joyce2016, though I end up drawing a slightly different conclusion to Joyce about the argument. And I'm going to start with a simplified version of the example, where I think it's clearer where things go wrong, and build up to the version Ahmed gives. (I've also relabeled the moves, because I find the labels here more intuitive.)

The simple version of Ahmed's example, which we'll call Betting Against the Demon, has three stages.

Stage One

:   Demon predicts whether Chooser will choose 1 box or 2 boxes at stage two.

Stage Two

:   Chooser chooses 1 box or 2 boxes. They receive \$100 if Demon predicted they will choose 1 box, and nothing if Demon predicted they will choose 2 boxes, whatever they choose. This payout is not revealed to them until the end.

Stage Three

:   Chooser selects one of two bets. Bet R (for right) wins \$25 if Demon predicted correctly, and loses \$75 if Demon predicted incorrectly. Bet W (for wrong) wins \$75 if Demon predicted incorrectly, and loses \$25 if Demon predicted correctly.

After this the moves are revealed, and Chooser gets their rewards. The strategic form of the game is shown in figure \@ref(fig:betting-against-demon). I'm assuming Demon wants to make correct predictions, and ignoring strategies that differ only in what Chooser does in worlds that are ruled out by their Stage Two choice. (I'll leave it as an exercise for the reader to confirm that they aren't relevant to the analysis.)

```{r, betting-against-demon, cache=TRUE}
demon_betting_strategic <- tribble(
	   ~"", ~P1, ~P2,
	   "1R", "125,1", "-75,0",
	   "1W", "75,1", "75,0",
	   "2R", "25,0", "25,1",
	   "2W", "175,0", "-25,1"
	)
gameformat(demon_betting_strategic, "The strategic form of Betting Against the Demon")
```

For Chooser, each row sets out what they do at stage two, and what they do at stage three - the number is whether they pick 1 box or 2, and the letter is whether they take bet R or W. For Demon, P1 is that they predict 1 box, and P2 is that they predict 2 boxes.

It should be clear enough that there are no pure strategy Nash equilibria for this game. The best response to P1 is 2W, but the pair of 2W and P1 is not a Nash equilibrium. And the best response to P2 is 1W, but the pair of 1W and P2 is not a Nash equilibrium.

But of course this game is not a strategic form game, it's an extensive form game. Figure \@ref(fig:betting-against-demon-tree) is the game tree for it.

```{tikz, betting-against-demon-tree, fig.cap = "The game tree for the betting against the demon example", fig.ext = 'png', cache=TRUE, fig.width = 4}
\usetikzlibrary{calc}
\begin{tikzpicture}[scale=1.4,font=\footnotesize]
\tikzset{
% Two node styles for game trees: solid and hollow
solid node/.style={circle,draw,inner sep=1.5,fill=black},
hollow node/.style={circle,draw,inner sep=1.5},
square node/.style={rectangle,draw, inner sep = 1, fill = black}
}

% Specify spacing for each level of the tree
\tikzstyle{level 1}=[level distance=22mm,sibling distance=25mm]
\tikzstyle{level 2}=[level distance=15mm,sibling distance=15mm]
\tikzstyle{level 3}=[level distance=17mm,sibling distance=10mm]
% The Tree
\node(0)[hollow node,label=below:{$Demon$}]{}
child[grow=left]{node(5)[solid node]{}
child[grow=up]{node(1)[solid node]{}
child{node[square node,label=above:{$125,1$}]{} edge from parent node [right]{$R$}}
child{node[square node,label=above:{$75,1$}]{} edge from parent node [left]{$W$}}
edge from parent node [left]{$1$}
}
child[grow=down]{node(3)[solid node]{}
child{node[square node,label=below:{$175,0$}]{} edge from parent node [left]{$W$}}
child{node[square node,label=below:{$25,0$}]{} edge from parent node [right]{$R$}}
edge from parent node [left]{$2$}
}
edge from parent node [below]{$P1$}
}
child[grow=right]{node(6)[solid node]{}
child[grow=up]{node(2)[solid node]{}
child{node[square node,label=above:{$-75,0$}]{} edge from parent node [right]{$R$}}
child{node[square node,label=above:{$75,0$}]{} edge from parent node [left]{$W$}}
edge from parent node [right]{$1$}
}
child[grow=down]{node(4)[solid node]{}
child{node[square node,label=below:{$-25,1$}]{} edge from parent node [left]{$W$}}
child{node[square node,label=below:{$25,1$}]{} edge from parent node [right]{$R$}}
edge from parent node [right]{$2$}
}
edge from parent node [below]{$P2$}
};

% information set
\draw[dashed](1) to (2);
\draw[dashed](3) to (4);
\draw[dashed, bend left](5) to (6);
% specify mover at 2nd information set
\node at ($(1)!.5!(2)$) [above]{$Chooser$};
\node at ($(3)!.5!(4)$) [below]{$Chooser$};
\node at ($(5)!.5!(6)$) [above, yshift = 25] {$Chooser$};
\end{tikzpicture}
```

Demon moves first, but Chooser is not alerted to Demon's move until the end of the game. So at every stage Chooser has two nodes in their information set - one for each of Demon's possible moves. But Chooser does know their own prior move, so at stage three the information sets do not include both the top and bottom of the tree. This looks a lot like a signaling game, but there are some notable differences. There is no move from Nature here; rather Demon moves once and Chooser moves twice. And there is an information set connecting the nodes in the middle-left and middle-right of the tree.

Since it is a game where Chooser moves twice, the relevant game theoretic concept to use is Bayesian Perfect Equilibrium. A strategy is defensible for Chooser iff it is part of a Bayesian Perfect Equilibrium. Since all Bayesian Perfect Equilibria are Nash Equilibria, and the game has no Nash Equilibria, there are no defensible moves. But the fact that it is a dynamic game is important to Ahmed's argument.[^demons-and-decisions-9]

[^demons-and-decisions-9]: I'm only going to discuss what Ahmed says about CDT combined with so-called 'sophisticated' choice, since it's the right way to make sequences of decisions.

Assuming that Chooser plays a pure strategy, in equilibrium Demon knows that strategy, and Chooser knows this. So at stage three, Chooser will believe that choosing R will win \$25 more or less for sure, and choosing W will lose \$25 more or less for sure. So Ahmed infers, correctly, that CDT says that Chooser should not choose W. From this he infers, incorrectly, that CDT says that Chooser should choose R.[^demons-and-decisions-10] This doesn't follow without an extra, incorrect, assumption that CDT says that something should be chosen in this case. But in fact, since the game has no pure strategy equilibria, that isn't true. Anyway, from that false assumption, Ahmed infers, correctly, that CDT recommends playing some strictly dominated strategy or other. And that's incoherent, since CDT is motivated by the thought that one should never play strictly dominated strategies.

[^demons-and-decisions-10]: Strictly speaking, what he shows is that this follows given that we are using the version of CDT described by @Lewis1981b. But he says earlier, in section 2.8 of his book, that he thinks all versions of CDT will agree with Lewis's version about everything that is covered in the book. As he puts it, "As far as this discussion is concerned, any one of them could stand for their disjunction." This is false, and importantly so at just this point. The version of CDT developed by @Skyrms1990 for instance would not agree with Lewis here, and would not say that any pure strategy is rational. Ahmed does discuss some of Skyrms's earlier work, but not the version of CDT that is developed from 1990 onwards using equilibrium concepts.

I've simplified matters a little here, but not in a way that matters. Ahmed doesn't say that Demon is arbitrarily accurate, but only that Demon is 80% accurate. But it's easy enough to modify the game to accommodate this. In fact there are two ways to do so, following the two tricks I introduced in section \ref(familiar). The Smullyan approach is to have a stage 0, where the Demon is assigned to one of two types: Knight or Knave. The former has an 80% chance of being the assignment. The Demon is told the assignment but Chooser is not. And if Demon is assigned Knave, their payouts are flipped - they get 1 if they play P1 and Chooser selects 2, or they play P2 and Chooser selects 1. The Selten approach is to have a stage 1A, between Demon and Chooser's moves. Demon chooses P1 or P2, then at stage 1A, there is a 20% chance that Nature will reverse Demon's choice, and the payouts will be a function not of what Demon did, but what happened after Nature modified Demon's choice. The trees for either case are impossibly messy, and I won't draw them. But it doesn't matter - the equilibrium analysis of either game is exactly the same as the equilibrium analysis of the game where Demon is arbitrarily accurate, and I'll stick with that game from now on.

So does CDT recommend playing a dominated strategy? Of course not. As Joyce says, CDT is "built to guarantee" that it doesn't recommend dominated strategies [@Joyce2016, 231]. What it says, or at least what causal defensivism says, is two-fold.

If Chooser can randomise, they should randomise. They should play the one and only equilibrium strategy in this game, which is a 50/50 mixture of 1W and 2W. In equilibrium, Demon will play a 50/50 mixture of P1 and P2. So at stage 3, Chooser will think it is 50% likely that Demon has 'predicted' correctly. (The scare quotes are because calling the output of Demon's mixed strategy a prediction stretches language just about to breaking point.) So Chooser will be happy sticking with their strategy at stage 3 - it will have an expected return of \$25, and the alternative has an expected return of -\$25.

If Chooser can't randomise, then it's a dilemma. What we say here should be similar to what EDT, or any other decision theory, says about the example in section \@ref(recipe). But what is that? Let's come back to that question in a bit - first I'll address some objections to the very idea that there could be dilemmas in decision theory.

# Against Coherence Norms {#coherence}

A common view in philosophy is that decision theory is the theory of how to make good decisions by one's own lights. Here's a particularly clear statement of this view by David Lewis. (The quote is from letter 695 in volume 2, to Jonathan Gorman, on page 472)

> The central question of decision theory is: which choices are the ones that serve one's desires according to one's beliefs?

Lewis was a causal decision theorist, but the view he's defending here is orthogonal to debates between causalists and evidentialists. To give it a name, I'll call it **coherentism about decision theory**, or coherentism for short. The coherentist believes, with Lewis, that a rational decision is one that best serves the chooser's ends given their actual beliefs.

The aim of this chapter is to argue against coherentism, and defend a **rationalist**. I'm following @Comesana2020 in saying that beliefs only rationalise actions if they are themselves rational. And note this is a sufficient condition for rationality, not a necessary one. I think actions can be decision theoretically rational even if the chooser hasn't formed the beliefs that rationalise them. The motivation for this is a much cited story by Frank Knight.

> Let us take Marshall's example of a boy gathering and eating berries ... We can hardly suppose that the boy goes through such mental operations as drawing curves or making estimates of utility and disutility scales. ... Nor is this any criticism of the boy. Quite the contrary! It is evident that the rational thing to do is to be irrational, where deliberation and estimation cost more than they are worth. [@Knight1921, 67]

But cases like this aren't going to be the main focus of attention here. Instead we're going to mostly talk about cases like Old Tom.

> **Old Tom**\
> Old Tom is punching himself in the face. He's doing this because he wants a nice cold beer, and believes, for no good reason whatsoever, that the only way to do this is to punch himself in the face. (And he wants the beer more than he wants to avoid being punched in the face.) His face hurts, and he has no beer.

I think that Old Tom isn't rational in any sense, including the decision theoretic sense. But I suspect many readers will disagree. Old Tom isn't doing well, those punches are going to hurt, but at least he's instrumentally rational.

I have four objections to this view. I'll run through two of them quickly here, and the third and fourth in subsequent sections.

First, I think that to the extent there is an intuition that Old Tom is doing well in *some* respect, it doesn't survive looking at a broader range of cases. So instead of the fictional case of Old Tom, consider the (more or less) realistic case of Dolores.[^demons-and-decisions-11].

[^demons-and-decisions-11]: For much more on this case, see @Keefe2018

> **Dolores**\
> Dolores wants the perfidious British to leave Northern Ireland. And she believes, for no good reason whatsoever, that the best way to bring this about is to set off car bombs on London streets. (And she desires the British exit from Northern Ireland more than she desires avoiding being someone who sets off car bombs.) So she sets of car bombs on London streets. This does not lead to the British leaving Northern Ireland.

I know intuitions differ on this, but I don't look at cases like this and think "At least she was being instrumentally rational." As the old saying goes, you do not under any circumstances have to hand it to the terrorists, even when you're talking about instrumental rationality. So I don't think that there is any intuitive sense in which Old Tom is doing something well. And since ideal decision theory is meant to be tracking something worth doing, I don't think it is tracking this kind of means-end coherence. That said, I know this is just a clash of intuitions and many readers won't agree with what I say about Dolores. So let's move on to other arguments.

Consider a variant of the Old Tom story where he also has the following beliefs, and has them because they are true and supported by his evidence.

-   It's Sunday, and if it's Sunday the best way to get a nice cold beer is to ask the local friendly barman (LFB).
-   It's raining, and if it's raining, the best way to get a nice cold beer is to ask LFB.
-   He's Scottish, and if he's Scottish, the best way to get a nice cold beer is to ask LFB.

Whether Old Tom asks LFB or punches himself in the face, he will be doing something a bit incoherent. Asking LFB is incoherent given his belief that the only way to get a nice cold beer is punching himself in the face. But not asking LFB is incoherent given each of these three beliefs I just listed. Indeed, not asking seems more incoherent, since it clashes with so many of his beliefs.

Now most coherentists are aware of this kind of case. They don't say Old Tom should punch himself. That's because they think that utilities and credences aren't really defined for cases like Old Tom's. Someone this incoherent doesn't really have a defined expected value for either punching himself in the face, or asking LFB. So it's not like the coherentist is committed to saying it is more coherent for Old Tom to punch himself in the face than ask LFB. But they are, at least as I read them, committed to saying that decision theory does not recommend asking LFB.

And that non-recommendation seems like a reductio of the view to me. For one thing, there is a clearly right thing to do here, both given the facts and given Old Tom's evidence and beliefs, and they are not recommending it. More importantly, it is now very hard to see what is interesting about the coherentist aim for decision theory. I can see, dimly, why one would care about making actions cohere with beliefs and desires. I can't see, at all, why one would care about a subject matter that at most says how someone who is already coherent in most respects can stay being coherent in other respects. That doesn't seem like a worthwhile project to me, and it's not the one I'm engaged in here.

## Coherence is a Substantive Norm

The coherentist is committed to saying that there need not be anything wrong with Old Tom in the following case. Old Tom has lost all the conditional beliefs about Sundays and Scotland, still believes the only way to get a nice cold beer is to punch himself in the face, but now has the following belief.

-   On each of the last one hundred days, Old Tom has wanted a nice cold beer. On each of those days, he has asked LFB for a beer, and LFB has given him a beer. He has no new evidence about LFB, but he no longer thinks this will work.

To make the case more substantive, imagine Old Tom has been reading two kinds of philosophical work. The first is work on inductive scepticism, and he has become convinced that the evidence of the last hundred days is not actually good evidence. The second is work on the harmony of the universe, and it has convinced Old Tom that you can only get something as good as a nice cold beer by doing something as unpleasant as punching yourself in the face. These are not great views, but they are coherent. And Old Tom is overall coherent, just in quite a bit of pain.

Or, at least, most coherentists say that they are coherent. To the extent I understand the notion of coherence, I think it's incoherent to know what happened for the last one hundred days, and not take it to be evidence for what will happen today. More generally, I think it's incoherent to believe against one's evidence. But the coherentist can't say that; if they do their view collapses into rationalism.

So the coherentist needs a way to distinguish coherence norms, like the norm against believing all of $p, p \rightarrow q$ and $\neg q$, from substantive norms, like the norm against being a counter-inductivist. One option is to try to argue that coherence norms are in some sense *formal*, while substantive norms are not. But this faces challenges in both directions. Some coherence norms, especially norms that cross attitude types, do not look particularly formal. And the view requires that the Carnapian project of understanding probability as truly logical can't work. And, for what it's worth, it's not like we have a particularly clear sense of what it is for a norm to be formal.

In recent work, Alex @Worsnip2021 has defended a different approach. He thinks what is definitive of incoherent attitudes is that having those attitudes, or at least having them "transparently" entails having a disposition to revise them.

> *Incoherence Test*. A set of attitudinal mental states is jointly incoherent iff it is (partially) constitutive of (at least some of) the states in the set that any agent who holds this set of states has a disposition, when conditions of full transparency are met, to revise at least one of the states. [@Worsnip2021, 132]

There is a rather obvious objection to this, which Worsnip is of course aware of. A lot of philosophers, especially logicians, defend views that most other philosophers would say are incoherent. This includes non-standard theories of decision, as in @Buchak2104, non-standard theories of uncertainty, as in @Shafer1977, and, especially, non-classical logics. It would be interesting, but take us too far afield, to see how Worsnip's account applies to intuitionist logicians.[^demons-and-decisions-12] So let's stick a case that clearly raises problems; what to say about dialethists such as Graham @Priest1987.

[^demons-and-decisions-12]: Or, in keeping with the spirit of this book, the intuitionist theory of uncertainty and decision set out in @Weatherson2005.

I'm not a dialethist, so I think Priest is making some kind of mistake. But if he is making a mistake, it is a subtle one, and one that differs in degree not in kind from the mistake that Old Tom makes when he endorses inductive scepticism.[^demons-and-decisions-13] Simplifying greatly, Priest believes that the following argument is basically sound.

[^demons-and-decisions-13]: To be clear, it differs in degree because it a much less culpable mistake than Old Tom's.

1.  The various semantic paradoxes should get a uniform solution.
2.  The best, perhaps only, uniform solution on the market is the dialethist solution.
3.  So, the dialethist solution to the semantic paradoxes is correct.
4.  So, both the Liar and its negation are true.

I'm a bit sceptical about premise 2; I'm not sold that the dialethist solutions to the Berry, Curry or Yablo paradoxes are particularly compelling, or uniform.[^demons-and-decisions-14] But I'm particularly sceptical about premise 1. And it's very notable for current purposes that if it is wrong, it's a mistake of substantive rationality, not coherence. It's an evidential principle that Priest is appealing to here, and that many may think are mistaken, just like Old Tom is appealing to an evidential principle when he goes for inductive scepticism.

[^demons-and-decisions-14]: To be very clear, this is not logical advice. Do not invest in logical theories on my say-so. I'm just noting, for the record, where *I* think the argument for dialethism goes wrong.

But all might feel too in the weeds. Isn't the simpler thing to say that if coherence norms are to have any bite at all, Priest's views must count as incoherent, but Priest violates the Incoherence Test? Worsnip is aware of this simple objection, and argues that the dialethist either doesn't really believe what they say, or they really do have the dispositions required by the test. Here's the key passage.

> Even if some dialetheists can have contradictory beliefs, I still suspect that in many cases this involves a kind of strain, or a sense of fighting oneself. In her darkest moments in the middle of the night, when the dialetheist is really feeling the power of the considerations favoring her belief in $p$, can she really avoid feeling at least *some* pressure---arising from these very same considerations---to give up her belief in not-$p$? [@Worsnip2021, 142 emphasis in original]

As a matter of observation, the dialethists I know sure don't seem to feel any such pressure. But who knows what goes through a logician's mind in the darkest moments in the middle of the night? So let's think about whether they should feel such pressure. Remember the key case here. It is when $p$ is the proposition that the negation of $p$ is true. Our dialethist is committed to $p$, and is thinking about the reasons why it is true. Should these considerations make them give up their belief in $\neg p$? Not at all! Given the T-schema, which they are very committed to, the truth of $p$ entails the truth of $\neg p$.

Now one might think at this point that the dialethist's appeal to the unrestricted T-schema is a mistake. And I wouldn't entirely disagree. But it's very hard to see how it is a formal mistake; it looks just the same kind of substantive mistake as Old Tom makes with inductive scepticism (though a much more plausible mistake than his). So if the dialethist goes wrong here, they go wrong in exactly the same way that someone who makes a substantive mistake goes wrong. So that shouldn't, and doesn't trigger the revision dispositions that Worsnip needs.

This is hardly a comprehensive survey of the field. But I'm left rather sceptical that the distinction the coherentist needs, between coherence norms and evidential, or substantive, norms, can be made out. I'll end this chapter by turning away from very general features of coherence norms, and looking at a particular reason for thinking coherence isn't all there is to decision theory.

## Coherence in Signaling Games

The argument in this section is that the best solution to the beer-quiche game [@ChoKreps1987] requires that we look at the rationality of the underlying beliefs, not just at which actions flow in the right way from existing beliefs. I'll start by describing the original game that Cho and Kreps use, then translate it into decision theoretic language, then argue that the coherentist solution to the game is implausible.

The back story is that Chooser has wandered into a disreputable bar by mistake, and now has a choice of ordering beer or quiche. Chooser doesn't have enough money for both, and Barman will be unhappy if they order neither. But Barman isn't the main foil for Chooser in our story; that instead would be Patron. The following facts are common knowledge between Chooser and Patron.

-   Chooser is either Tough or Wimp.
-   Chooser knows which of these they are, Patron does not. Patron's prior credence that Chooser is Tough is 0.6, leaving 0.4 credence that Chooser is Wimp.[^demons-and-decisions-15]
-   If Chooser is Tough, they prefer beer. If Chooser is Wimp, they prefer quiche.
-   Patron is a bully who will fight Wimps but not Toughs. They utility 1 from fighting Wimps and not fighting Toughs, and utility 0 from the converse.
-   Chooser's order will become common knowledge, and Patron will make their decision about whether Chooser is Tough or Wimp after hearing it.
-   Chooser gets utility 1 from ordering their preferred option (beer or quiche), and utility 2 from Patron not fighting them.

[^demons-and-decisions-15]: Just how these facts about Patron's credences become common knowledge is not made entirely clear in the story, but let's pretend they are.

So here is the game tree.

```{tikz, beer-quiche, fig.cap = "Tree Diagram of the Beer-Quich Game", fig.ext = 'png', cache=TRUE, fig.width = 4}
\usetikzlibrary{calc}

\begin{tikzpicture}[scale=1.4,font=\footnotesize]
\tikzset{
% Two node styles for game trees: solid and hollow
solid node/.style={circle,draw,inner sep=1.5,fill=black},
hollow node/.style={circle,draw,inner sep=1.5},
square node/.style={rectangle,draw, inner sep = 1, fill = black}
}

% Specify spacing for each level of the tree
\tikzstyle{level 1}=[level distance=12mm,sibling distance=25mm]
\tikzstyle{level 2}=[level distance=15mm,sibling distance=15mm]
\tikzstyle{level 3}=[level distance=13mm,sibling distance=11mm]

% The Tree
\node(0)[hollow node,label=above:{Nature}]{}
child[grow=left]{node[solid node,label=left:{
$Chooser$
}] {}
child[grow=up]{node(1)[solid node]{}
child{node[square node,label=above:{$1,0$}]{} edge from parent node [right]{$W$}}
child{node[square node,label=above:{$3,1$}]{} edge from parent node [left]{$T$}}
edge from parent node [left, yshift = -5]{$Beer$}
}
child[grow=down]{node(3)[solid node]{}
child{node[square node,label=below:{$2,0$}]{} edge from parent node [left]{$T$}}
child{node[square node,label=below:{$0,0$}]{} edge from parent node [right]{$W$}}
edge from parent node [left, yshift = 5]{$Down$}
}
edge from parent node [below, align=center]{$Tough$ \\ $0.6$}
}
child[grow=right]{node[solid node,label=right:{
$Chooser$
}] {}
child[grow=up]{node(2)[solid node]{}
child{node[square node,label=above:{$0,1$}]{} edge from parent node [right]{$W$}}
child{node[square node,label=above:{$2,0$}]{} edge from parent node [left]{$T$}}
edge from parent node [right, yshift = -5]{$Up$}
}
child[grow=down]{node(4)[solid node]{}
child{node[square node,label=below:{$3,0$}]{} edge from parent node [left]{$T$}}
child{node[square node,label=below:{$1,1$}]{} edge from parent node [right]{$W$}}
edge from parent node [right, yshift = 5]{$Down$}
}
edge from parent node [below,align=center]{$Wimp$ \\ $0.4$}
};

% information set
\draw[dashed,rounded corners=10]($(1) + (-.45,.45)$)rectangle($(2) +(.45,-.45)$);
\draw[dashed,rounded corners=10]($(3) + (-.45,.45)$)rectangle($(4) +(.45,-.45)$);
% specify mover at 2nd information set
\node at ($(1)!.5!(2)$) {$Patron$};
\node at ($(3)!.5!(4)$) {$Patron$};
\end{tikzpicture}


```

The game starts in the middle. Nature assigns Chooser to a type: Tough or Wimp. Then Chooser orders, moving up for beer and down for quiche. Then Patron decides Chooser is Tough, and doesn't fight them, or Wimp, and does. And then we get the payoffs. The dashed circles indicate that when Patron moves, they don't know which of two nodes they are at.

This is of course a signaling game of the kind introduced to the literature by David @Lewis1969. It's not the kind of cooperative signaling game that Lewis was most interested in. But still the structure is that one player gets a message from nature, then makes a signal, and the second player reacts in part by trying to deduce what the signal means about what the first player learned.

This game has two equilibria. Both of them are *pooling* equilibria; Chooser plays the same thing whatever type they are.

The natural equilibrium is that Chooser orders beer whatever type they are, and Patron fights quiche eaters and not beer drinkers. In practice, since Patron doesn't get any information from Chooser's order, their posterior credence that Chooser is Tough equals their prior credence, i.e., 0.6. So Patron doesn't fight, and we end up at either the 3,1 or 2,0 outcomes. In expectation, Chooser's return is 2.6, and Patron's return is 0.6. But neither can do any better given the other's move. If Chooser deviates to quiche when they are Wimp, Patron will fight, and Chooser's payoff will fall from 2 to 1. And Patron is utility maximizing given their evidence.

But there is another, less natural, equilibrium. Chooser orders quiche whatever type they are, and Patron is disposed to fight beer orderers and not fight quiche orderers. Chooser's expected return is only 2.4 in this equilibrium. But neither when they are Tough nor when they are Wimp can they do better given Patron's dispositions. And Patron maximizes expected utility---again, they have to use their prior probability of Chooser's type since they get no useful information. So everything is stable and coherent.

This equilibrium is very strange, because it's hard to make sense of Patron's mindset. And it is harder still to make sense of why Chooser would think that Patron has that mindset. Patron has to think the following things at once. Whether Chooser is Tough or Wimp, they'd be better off ordering quiche, given Patron's dispositions. But, if one of the two were to do the irrational thing, it would be Wimp. And that's true even though Wimp is currently getting their highest possible payout, i.e., three, while Tough is only getting two and could arguably think it was worth the gamble to order beer. And then Chooser has to believe all of this about Patron, with very little evidence.

This game can quite easily be turned into a decision problem. Simply make Patron into Demon with the following characteristics. They want to predict Chooser's type after seeing Chooser's order. They are arbitrarily good at predicting Chooser's *strategy*, i.e., Chooser's dispositions for what to order if Tough or Wimp. They are no better than chance at predicting whether Chooser is Tough or Wimp to start with, but they do know that the probability that Chooser was assigned Tough is 0.6. Then everything else stays the same. Chooser is told their type, then Chooser orders, then Demon says whether Chooser is Tough or Wimp. Demon wants to predict correctly; Chooser wants to maximize utility. Chooser gets 1 for the right order, and 2 for Demon saying they are Tough. Demon wants to get it right, but can't do much if Chooser plays a pooling strategy.

And again, there is a question about what if anything is wrong with the pooling strategy of Chooser always selecting quiche. It maximizes utility if Chooser believes that Demon will say that Chooser is Tough if they order quiche, and Wimp if they order beer. And that belief isn't strictly wrong; in the only situation that obtains, it is a correct belief about what Demon will do.

Still, it seems intuitively wrong. It isn't rational to order quiche no matter what. And this isn't just my intuition. Cho and Kreps report the same intuition; about the game version to be sure not the decision version, but it's essentially the same. And so do the literally thousands of writers who have follow Cho and Kreps. They develop something they call the *intuitive criterion* to rule out the all-quiche strategy. The literature on this is voluminous, and I'm not going to summarise it here. But what's important is that this criterion is usually taken to be, usually presented as, continuous with other criteria like subgame perfection that can be used to determine the rational solution in simpler games. That is, it is a criterion of game-theoretic, and hence decision-theoretic, rationality.

But it's not a criterion that the coherentist can accept. There is nothing incoherent about the belief set that leads to the all-quiche strategy. If Chooser believes that Demon/Patron is following the strategy Tough-if-quiche/Wimp-if-beer, all-quiche is the rational play. And that belief is coherent with their other attitudes. It just isn't evidentially rational. There is no reason to think that that's what Demon/Patron would do if beer were selected.

So decision theory should say in this case that Chooser should order beer. Coherentist decision theory cannot make that recommendation. It says that Chooser should play a pooling strategy but doesn't say which pooling strategy to play. It is only when we supplement coherentist decision theory with the requirement that beliefs are rational that we get the desired result that Chooser should order beer. So coherentist decision theory is false, and its rival rationalist decision theory is true.

Part of what's philosophically interesting about Cho and Kreps's work is that it shows that the following two categories are not equivalent.

-   Criteria of rationality that are to do with coherence, as opposed to substantive rationality.
-   Criteria of rationality that can given a precise mathematical formulation.

It's easy to think these go together because most of the criteria of rationality that we discuss in philosophy fall into both or neither. Criteria to do with credences conformity to the probability calculus fall into both. Criteria to with the requirement that credences are (in some sense) proportional to the evidence are in neither. But the criterion that Cho and Kreps offer to rule out the all quiche strategy has a precise mathematical formulation, even though it is clearly a substantive not a coherence constraint. There is nothing incoherent about Chooser believing that Demon/Patron will (coherently) predict that quiche orderers are Tough and beer orderers are Wimps. But it is substantively irrational, and in conflict with the so-called "intuitive criterion".

# Responding to Evidential Decision Theory {#antiedt}

## So Why Ain't You Rich? {#antiwar}

There is a familiar complaint against causal decision theory that goes back to the modern origins of decision theory in the 1970s. Here is a recent version of it due to @AhmedPrice2012. While their version is primarily directed against proceduralist forms of causal decision theory, this particular objection does not turn on the proceduralism. If the objection works, it also works against my defensivist version of causal decision theory. (I've slightly changed some of the wording, but otherwise this argument is quoted from page 16 of their paper.)

1.  In Newcomb problems, the average returns to one-boxing exceed that to two-boxing.
2.  Everyone can see that (1) is true.
3.  Therefore one-boxing foreseeably does better than two-boxing. (by 1, 2)
4.  Therefore Causal Decision Theory (CDT) is committed to the foreseeably worse option for anyone facing Newcomb's problem.

Here's what they, and many other proponents of Evidential Decision Theory (EDT) say follows from 4.

> The point of the argument is that if everyone knows that the CDT-irrational strategy will in fact do better on average than the CDT-rational strategy, then it's rational to play the CDT-irrational strategy. [@AhmedPrice2012, 17]

This is what @Lewis1981e called the "Why Ain'cha Rich" argument, and what following @Bales2018 I'll call the WAR argument. I'm going to argue the last step of the WAR argument doesn't follow. Or, at the very least, that proponents of EDT cannot coherently say that it follows. For there are several cases where EDT foreseeably does worse than CDT. This section will go over three of them.

### Example One - Split Newcomb

This game takes place over three rounds.

1.  At stage one, the human player chooses Play or Exit. If they choose Out, player gets 5 and demon gets 1. If they choose In, we move onto stage two.
2.  At stage two, demon chooses Left or Right, and this choice is announced.
3.  At stage three demon and the player simultaneously choose either Up or Down. Demon is very good at predicting what player's choices will be, and indeed at stage two they were already very good at making such a prediction. And Demon wants to use these predictive powers to get as high a payoff as possible, and this is common knowledge.

If Demon chose Left at stage two, stage three involves the game in table \@ref(tab:left-anti-war).

```{r left-anti-war, cache=TRUE}
left_anti_war <- tribble(
	   ~"", ~PU, ~PD,
	   "U", "$2, 1$", "$4, 0$",
	   "D", "$1, 0$", "$3, 3$"
	)
gameformat(left_anti_war, "The left hand side of Split Newcomb")
```

But if Demon chose Right at stage two, stage three involves the game in table \@ref(tab:right-anti-war).

```{r right-anti-war, cache=TRUE}
right_anti_war <- tribble(
	   ~"", ~PU, ~PD,
	   "U", "$12, 4$", "$14, 0$",
	   "D", "$11, 0$", "$13, 2$"
	)
gameformat(right_anti_war, "The left hand side of Split Newcomb")
```

If you'd prefer it as a game tree, it is presented in figure \@ref(fig:first-anti-war).

```{tikz, first-anti-war, fig.cap = "Tree Diagram of the Split Newcomb Game", fig.ext = 'png', cache=TRUE, fig.width=5}
\tikzset{
    % Three node styles for game trees: solid, hollow, square
    solid node/.style={circle,draw,inner sep=1.5,fill=black},
    hollow node/.style={circle,draw,inner sep=1.5},
    square node/.style={rectangle,draw, inner sep = 1, fill = black}
}

\usetikzlibrary{calc}

\begin{tikzpicture}[scale=1.5,font=\footnotesize]
  % Specify spacing for each level of the tree
    \tikzstyle{level 1}=[level distance=15mm,sibling distance=45mm]
    \tikzstyle{level 2}=[level distance=15mm,sibling distance=45mm]
    \tikzstyle{level 3}=[level distance=15mm,sibling distance=10mm]
    \tikzstyle{level 4}=[level distance=12mm,sibling distance=10mm]
  % The Tree
  \node(0)[hollow node,label=left:{$C$}]{}
     child[grow=down]{node[solid node,label=below:{$(5,1)$}]{} 
        edge from parent node[right]{$E$}
        }
     child[grow=up]{node[solid node,label=above:{$D$}]{} 
        child[grow=left]{node[solid node,label=left:{$D$}]{} 
            child[grow=north west]{node(5)[solid node]{}
                child[grow=150]{node(1)[square node, label=left:{2,1}]{}
                    edge from parent node[above]{$U$}
                }            
                child[grow=210]{node(2)[square node, label=left:{1,0}]{}
                    edge from parent node[below]{$D$}
                }            
                edge from parent node[right]{$PU$}
            }
            child[grow=south west]{node(6)[solid node]{} 
                child[grow=150]{node(3)[square node, label=left:{4,0}]{}
                    edge from parent node[above]{$U$}
                }            
                child[grow=210]{node(4)[square node, label=left:{3,3}]{}
                    edge from parent node[below]{$D$}
                }            
                edge from parent node[right]{$PD$}
            }
            edge from parent node[above]{$L$}
        }
        child[grow=right]{node[solid node,label=above:{$D$}]{} 
            child[grow=north east]{node(11)[solid node]{}
                child[grow=30]{node(7)[square node, label=right:{12,4}]{}
                    edge from parent node[above]{$U$}
                }            
                child[grow=330]{node(8)[square node, label=right:{14,0}]{}
                    edge from parent node[below]{$D$}
                }            
                edge from parent node[left]{$PU$}
            }
            child[grow=south east]{node(12)[solid node]{} 
                child[grow=30]{node(9)[square node, label=right:{11,0}]{}
                    edge from parent node[above]{$U$}
                }            
                child[grow=330]{node(10)[square node, label=right:{13,2}]{}
                    edge from parent node[below]{$D$}
                }            
                edge from parent node[left]{$PD$}
            }
            edge from parent node[above]{$R$}
        }
        edge from parent node[left]{$P$}
      }
;

  \draw[dashed,rounded corners=10]($(5) + (-.2,.25)$)rectangle($(6) +(.2,-.25)$);
  % specify mover at 2nd information set
  \node at ($(5)!.5!(6)$) {$C$};

  \draw[dashed,rounded corners=10]($(11) + (-.2,.25)$)rectangle($(12) +(.2,-.25)$);
  % specify mover at 2nd information set
  \node at ($(11)!.5!(12)$) {$C$};

\end{tikzpicture}

```

We start at the hollow node in the middle, and Chooser (here denoted as 'C') either goes up by Playing, or goes down by Exiting. Then Demon moves Left or Right. Then Demon moves again, making a prediction. But this second move isn't revealed to Chooser, which is why on either side Chooser's nodes are in an information set. That's to say, Chooser chooses Up or Down knowing whether Demon has gone Left or Right, but not knowing whether Demon has predicted Up or Down. And then we get the payoffs.

Whether Demon goes Left or Right, the CDTer will choose Up, and the EDTer will choose Down. Either choice Chooser faces is a fairly straightforward Newcomb Problem. In both sub-games Up causally dominates Down, but Down will get a higher return if you assume, as we did assume, that demon mostly makes correct predictions.

So at stage two, Demon will know that if the person facing them is an EDTer, they will get a return of 3 from Left and 2 from Right. (They'll end up in the Down-Down cell either way.) So they will rationally choose Left. On the other hand, if the person facing them is a CDTer, they will get a return of 1 from Left and 4 from Right. (They'll end up in the Up-Up cell either way.) So they will rationally choose Right. And everything in this paragraph can be deduced by a rational player at stage 1.

So at stage one, a CDTer will know that if they Play, they expect to get 12 (the game will go Right then Up-Up), and if they Exit, they know they'll get 5. So they'll Play. But an EDTer will know that if they Play, they expect to get 4 (the game will go Left then Down-Down), and if they Exit, they know they'll get 5. So they'll Exit.

The result of all this is that the CDTer will get 12, and the EDTer will get 5. So the CDTer will predictably do better than the EDTer. Indeed, the EDTer will voluntarily choose at stage one to take a lower payout than the CDTer ends up with. This seems bad for EDT, at least if we think that predictably ending up with a lower outcome is bad.

Now you might object that this is because at stage two the demon chooses to treat the EDTer differently to how they treat the CDTer. I don't really agree for two reasons, though I'm not sure either of these reasons work. (Hence the second and third examples that are about to come.) One is that the demon isn't trying to harm the EDTer; they are just trying to maximise their return. It so happens that EDT is such an impermissive theory that it doesn't allow for any flexibility, and the Demon, knowing this, is forced to take choices that are bad for EDT, and indeed worse for Demon than if they ended up at Right-Up-Up. But this isn't Demon's fault; it's the fault of EDT being so impermissive. The other reason is that Demon does not in fact make any choices that hurt the EDTer. The EDTer should expect that Demon will in fact make such choices, in response to their theory, but that's not quite the same thing. The only player who moves at all in the EDT version of the game is Chooser. So it's a little hard to say this is just a case where the EDTer is harmed by the demon's malicious choices.

I think those responses work, but I'm not completely sure that they do. So let's look at a different example, one where Demon doesn't have these variable payouts.

### Example Two - Coins and Signals {#coinssignals}

This example is a version of a signaling game of the kind introduced by @Lewis1969a. And in particular it's a version of the broadly adversarial kinds of signaling games that are central to the plot of @ChoKreps1987, and which we discussed a lot in chapter \ref\@(coherent). Again, it will involve three stages.

At the first stage a fair coin is flipped, and the result shown to Chooser, but not to Demon.

At the second stage, Chooser will choose Up or Down, and the choice will be publicly announced.

At the third stage, Demon will try to guess what the coin showed. Demon knows the payoff table I'm about to show you, and is arbitrarily good at predicting Chooser's strategy. That is, Demon can make accurate predictions of the form "If Heads, Chooser will make this choice, and if Tails, they will make that choice."

The payoffs to each player are a function of what happens at each of the three steps, and are given by the following table.

| Coin | Chooser | Demon | Chooser Payoff | Demon Payoff |
|:----:|:-------:|:-----:|:--------------:|:------------:|
|  H   |    U    |   H   |       40       |      1       |
|  H   |    U    |   T   |      400       |      0       |
|  H   |    D    |   H   |       0        |      1       |
|  H   |    D    |   T   |       0        |      0       |
|  T   |    U    |   H   |       40       |      0       |
|  T   |    U    |   T   |       28       |      1       |
|  T   |    D    |   H   |       28       |      0       |
|  T   |    D    |   T   |       36       |      1       |

: (#tab:payoffs-demon-coin) Payoffs for the coins and signals game.

Figure \@ref(fig:second-anti-war) shows game they are playing in tree form.

```{tikz, second-anti-war, fig.cap = "Tree Diagram of the Coins and Signals Game", fig.ext = 'png', cache=TRUE, fig.width=4}
\usetikzlibrary{calc}

\begin{tikzpicture}[scale=1.4,font=\footnotesize]
\tikzset{
% Two node styles for game trees: solid and hollow
solid node/.style={circle,draw,inner sep=1.5,fill=black},
hollow node/.style={circle,draw,inner sep=1.5},
square node/.style={rectangle,draw, inner sep = 1, fill = black}
}

% Specify spacing for each level of the tree
\tikzstyle{level 1}=[level distance=12mm,sibling distance=25mm]
\tikzstyle{level 2}=[level distance=15mm,sibling distance=15mm]
\tikzstyle{level 3}=[level distance=13mm,sibling distance=11mm]

% The Tree
\node(0)[hollow node,label=above:{Nature}]{}
child[grow=left]{node[solid node,label=left:{
$Chooser$
}] {}
child[grow=up]{node(1)[solid node]{}
child{node[square node,label=above:{$400,0$}]{} edge from parent node [right]{$T$}}
child{node[square node,label=above:{$40,1$}]{} edge from parent node [left]{$H$}}
edge from parent node [left, yshift = -5]{$Up$}
}
child[grow=down]{node(3)[solid node]{}
child{node[square node,label=below:{$0,1$}]{} edge from parent node [left]{$H$}}
child{node[square node,label=below:{$0,0$}]{} edge from parent node [right]{$T$}}
edge from parent node [left, yshift = 5]{$Down$}
}
edge from parent node [below, align=center]{$H$ \\ $0.5$}
}
child[grow=right]{node[solid node,label=right:{
$Chooser$
}] {}
child[grow=up]{node(2)[solid node]{}
child{node[square node,label=above:{$28,1$}]{} edge from parent node [right]{$T$}}
child{node[square node,label=above:{$40,0$}]{} edge from parent node [left]{$H$}}
edge from parent node [right, yshift = -5]{$Up$}
}
child[grow=down]{node(4)[solid node]{}
child{node[square node,label=below:{$28,0$}]{} edge from parent node [left]{$H$}}
child{node[square node,label=below:{$36,1$}]{} edge from parent node [right]{$T$}}
edge from parent node [right, yshift = 5]{$Down$}
}
edge from parent node [below,align=center]{$T$ \\ $0.5$}
};

% information set
\draw[dashed,rounded corners=10]($(1) + (-.45,.45)$)rectangle($(2) +(.45,-.45)$);
\draw[dashed,rounded corners=10]($(3) + (-.45,.45)$)rectangle($(4) +(.45,-.45)$);
% specify mover at 2nd information set
\node at ($(1)!.5!(2)$) {$Demon$};
\node at ($(3)!.5!(4)$) {$Demon$};
\end{tikzpicture}


```

Demons's payoffs are just as you'd expect - they get rewarded iff they figure out how the coin landed. Chooser's payoffs are more complicated, but the big thing to note is they get the biggest rewards if they manage to play Up while Demon makes an incorrect prediction.

One last thing to stipulate about Demon before we analyse the game. If Demon predicts Chooser will do one thing if Heads and another if Tails, they will use the information from Chooser's choice to make their guess about how the coin landed. But if they predict Chooser will say the same thing whether the coin landed Heads or Tails, they won't know how the coin landed, and will flip their own coin to make a guess. So in that case it will be 50/50 whether Demon says Heads or Tails.

Onto the analysis. It should be fairly clear that if the coin lands Heads, the human should say Up. The worst possible return from Up is 40, the best possible return from Down is 0. So that's what both a CDTer and an EDTer would do, and hence what Demon would predict that they will do.

So what happens if the coin lands Tails? Given Demon will predict Up if Heads, we can work out the value of Up and Down if Tails to the EDTer. If they play Up, Demon will predict that, and hence Demon will flip a coin to choose Heads or Tails. So they have a 50/50 shot at getting either 40 or 28, and so their expected return is 34. If they play Down, Demon will predict that, and hence Demon will say Tails, and they will get a return of 36. Since 36 \> 34, they will play Down if Tails.

That's the unique solution to the game for the EDTer. They play Up if Heads, Down if Tails. Demon can figure out that they'll do this, so will correctly guess what the coin showed. And they will get 40 if the coin landed Heads, and 36 if it landed Tails, for an expected return of 38.

What should the CDTer do? And, in particular, what should a causal defensivist do? Well, it turns out this is another problem where the theory is not decisive. Doing exactly what the EDTer does is defensible. But it's also defensible to say Up no matter what. Let's go over why this is defensible. The question is whether Chooser can endorse their decision to play Up no matter what after each possible result of the coin toss. They can clearly endorse it if the coin lands Heads; in that case Up strictly dominates Down, and strictly dominant are always defensible. What if the coin lands Tails? Well they think they'll play Up. So they think the demon will flip a coin to guess in this situation. So they think the expected return of Up is 34 (like the EDTer thinks), and the expected return of Down is 32. The key difference here is that when working out the expected return of a non-chosen option, the Chooser who believes in causal defensivism does not change the expected behavior of the demon, while the EDTer does. (This disposition is why dominance reasoning works for them.) So Chooser will think that even if the coin lands Tails, they would do worse on average if they switched to playing Down if Tails. So it follows that they can defensibly play Up either way.

And if they do play this, the rewards are handsome. The demon won't have any information about the coin, so the demon will flip their own coin. So lines 1, 2, 5 and 6 of the table are all equally likely to appear. So if Chooser plays this strategy, they are equally likely to get a return of 40, 400, 40 or 28, for an overall expected return of 127. And this is much higher than the 38 the EDTer is expected to receive. By changing the payout on line 2 of the table, we can make the gap in expected returns be arbitrarily large.

Now you might object that while the the causal defensivist can do better, it doesn't follow that EDT is wrong. After all, we've just said here that a rival theory may do better. I don't think that matters much. The point of the WAR is to refute a theory, and if the EDTer does foreseeably worse than one kind of Chooser who follows causal defensivism, that should be enough to refute them. But just in case you think this objection is stronger, we'll include one last example.

### Example Three - Coins and Newcomb

This is just like Example Two, with one twist. If the game goes Tails, Down, Tails, then we don't immediately end the game and make payouts to the players. Instead we play another game, with a familiar structure, and the payouts shown in table \@ref(tab:third-anti-war). As always, Demon is really good at predicting Chooser's play, and Chooser's payouts are listed first in every cell. (I'm not going to include a tree here, because it is more confusing than helpful.)

```{r third-anti-war, cache=TRUE}
third_anti_war <- tribble(
	   ~"", ~PU, ~PD,
	   "U", "$20, 1$", "$40, 0$",
	   "D", "$16, 0$", "$36, 1$"
	)
gameformat(third_anti_war, "The payouts after tails–down–tails in coins and signals.")
```

The EDTer will think they'll get 36 from this game, so the example will be just like Example Two. And the EDTer will play Up if Heads, Down if Tails, for an expected return of 38.

But if Chooser follows causal defensivism, then they will think that if the game gets to this stage, they'll get 20. So now they think that in the original game, Up dominates Down no matter whether the coin lands Heads or Tails. So now they will definitely play Up no matter what, and get an expected return of 127.

### Why The Examples Matter

I've argued against EDT elsewhere in the book, but note that this section is very much not an argument against EDT; instead, it's part of a war on WAR. The point of the first example is that any theory whatsoever is subject to a WAR argument. That's because for any theory whatsoever, you can construct pairs of choices like Left and Right, where the theory says to take choices that lead Demon to preferring to go Left. So for any theory whatsoever, or at least any theory that is consequentialist in the sense popularised by @Hammond1988, there is an example where the theory leads to worse returns. So any consequentialist theory is subject to an objection by WAR. It's the paradigm of an over-generating objection.

There is perhaps something a bit interesting about the second example, though it isn't a problem especially for EDT. What makes the second example work is that Chooser is in a situation that rewards unpredictability, but EDT is decisive, and hence predictable. And any decisive theory will be subject to an WAR-style objection from cases like this. Now this isn't part of my argument for indecisiveness, since I think WAR-style objections are bad. But following the recipe from subsection \@ref(coinssignals), it is possible to construct a case where any decisive theory will lead to predictably bad outcomes. It's always incoherent to endorse the combination of WAR reasoning and decisive decision theories. Even though I reject both of these, I think it's pretty clear in this case that WAR reasoning should be the first to go. And with it goes not EDT, but one of the historically key arguments for EDT. Let's turn to a more contemporary argument for EDT next.

## To Bet the Impossible Bet

The Ahmed cases all go away if you focus on what's possible at the end of deliberation, not at the start of it

This might require some contextualism about ability, a la Hawthorne and Pettit, to really make stick

See page 63 of Joyce book for the premise that counterfactuals about the bet must be specified in order for us to have a real bet.

See also Joyce's review of Ahmed for much more on this

# Epistemic Uniqueness and Decisiveness

## Uniqueness and Permissivism

In chapter \@ref(decisive) I argued that decision theory is often compatible with multiple unequal solutions to games. And in chapter \@ref(coherence) I argued that decision theory should be sensitive to what credences the chooser should have, not just the credences they actually have. If you add to these views the philosophical view known as Uniqueness, you get something close to a contradiction. Uniqueness says, roughly, that in any situation there is a unique set of credences one should have. To keep my views coherent, I need to endorse its negation: Permissivism, which says that rationality is compatible with a number of distinct attitudes in particular situations.

This chapter argues that thinking about symmetric games gives us new reason to believe in permissivism. In some finite games, if permissivism is false then we have to think that a player is more likely to take one option rather than another, even though each have the same expected return given that player's credences. And in some infinite games, if permissivism is false there is no rational way to play the game, although intuitively the games could be rationally played. The latter set of arguments rely on the recent discovery that there are symmetric games with only asymmetric equilibria. It was long known that there are symmetric games with no pure strategy symmetric equilibria; the surprising new discovery is that there are symmetric games with asymmetric equilibria, but no symmetric equilibria involving either mixed or pure strategies.

The permissivist theses that have been the focus of recent philosophical attention vary along two dimensions.[^demons-and-decisions-16]

[^demons-and-decisions-16]: For a much more thorough introduction to the debate, and especially into the varieties of permissivist theses, see @KopecTitelbaum2016. Much of the setup here, including for example the use of the subjective Bayesian as an illustrative example, is from that paper. Some notable more recent contributions to the debate include @Schultheis2018 arguing for Uniqueness, and @Callahan2021 arguing for Permissivism. Callahan connects Permissivism to existentialism. I suspect there are deep and unexplored connections here, but exploring them is a task for a different book.

The first dimension concerns what we hold fixed when we say that multiple attitudes are rationally permissible. The weakest possible theory just says that two people with distinct attitudes may both be rational. No one really denies this. The strongest theory says that holding every fact about a situation constant, there are two possible rational attitudes. In between we have a number of interesting theses. For instance, we can ask whether multiple attitudes are rationally compatible holding constant the evidence the believer has. And we can ask whether multiple attitudes are rationally compatible holding constant both the evidence and the believer's prior doxastic states. A classic form of subjective Bayesianism answers *yes* to the first question, and *no* to the second. The focus here will be on a thesis very close to the strongest one - whether two people who are alike in all qualitative respects can rationally have different attitudes.

The second dimension concerns whether the folks holding these distinct attitudes can acknowledge that rival attitudes are rational. Some permissivists hold that distinct attitudes can be rationally compatible with holding fixed evidence or priors or whatever, but the people holding these attitudes cannot acknowledge that attitudes other than theirs are rational. The argument I'm going to offer draws the stronger conclusion that multiple responses are rational, and rational thinkers can acknowledge that alternative responses to theirs are rational.

The negation of a permissive thesis is a Uniqueness thesis. The name suggests that there is precisely one rational attitude to take in a specified situation, but we'll interpret it as the view that there is at most one rational attitude to take so as to ensure each Uniqueness thesis is the negation of a permissive thesis. As with Permissivism, Uniqueness comes in weaker and stronger varieties. The strongest version is the literally incredible view that there is only one doxastic attitude that is rationally permissible. (Presumably it is the view that is certain of all and only truths.) The weakest version, which is still interesting, is that once a situation is described in full detail, there is precisely one doxastic attitude that is rationally permissible. Everyone holds that, since the normative supervenes on the descriptive, that describing a situation in full detail fixes which doxastic states are rationally permissible. The Uniqueness theorist adds the claim that there are 0 or 1 such states.[^demons-and-decisions-17]

[^demons-and-decisions-17]: Two small caveats here. Uniqueness theorists may say that it is permissible to not have any attitude towards a proposition. So it is consistent with Uniqueness, as I understand it, to say that it could be both rational to have credence 0.6 in $p$, and rational to not have any attitude towards $p$. What the Uniqueness theorist denies is that there are distinct credences towards $p$ one could adopt, each of which would be rational. And of course the Uniqueness theorist thinks it could be rational to have credence 0.6 in $p$ and 0.7 in $q$. When I say Uniqueness implies that just one state is rational, I mean to quantify over complete credal states, not attitudes towards single propositions.

As I said, I'm interested in defending a strong, but not maximally strong, version of Permissivism. Equivalently, I'm interested in attacking a weak, but not quite maximally weak, version of Uniqueness. Here is the version of Uniqueness that I want to reject.

-   For all kinds $K$, and evidence $E$, there is at most one credal distribution that is rational for an agent of kind $K$ with evidence $E$.

I mean to be fairly liberal over what counts as a \`kind', so if any such theory is false, we have proven that a lot of Uniqueness theories are false. So a kind could be a prior probability function, a set of privileged predicates that one uses for induction, an attitude to inductive risk, and so on. The only assumption I'll make is that kinds are shareable; so there is no such thing as the kind *Being John Malkovich*. In principle we could say that what evidence one has is part of one's kind, but the discussion below will be clearer if we separate out evidence and kinds.

The next two sections set out two symmetric games where Uniqueness leads to surprising results. I think the results are surprising, indeed implausible, enough that we should reject Uniqueness. But even if one doesn't accept that, it's still interesting to see what Uniqueness entails. In all cases we'll assume that the following things are common knowledge among players of the game.

-   Each player is rational, so they form rational credences, and maximise expected utility.
-   Each player is of kind $K$.
-   Each player knows the payout structure of the game.
-   Each player is self-aware; they know their own credences.
-   If Uniqueness is true, then each player knows that Uniqueness is true.
-   Each player has no other relevant evidence about the game or the players.

Let evidence $E$, unless otherwise stated, be the evidence specified by those six bullet points. We'll be continually thinking about propositions of the form:

-   A rational agent of kind $K$ with evidence $E$ will perform action $varphi$ in this game.

Since the games are symmetric, we don't have to ask about which player will make this move; we can think abstractly about what any rational player would do.

## Chicken

Some finite symmetric games have no symmetric pure-strategy equilibria. One notable example is Chicken. Table \@ref(tab:simple-chicken) shows a version of Chicken that will do.

```{r, simple-chicken}
chicken <- tribble(
	   ~"", ~Stay, ~Swerve,
	   "Stay", "-100, -100", "1, -1",
	   "2", "1, -1", "0, 0"
	)
gameformat(chicken, "Chicken")
```

The symmetric pure-strategy pairs $\langle$Stay, Stay$\rangle$ and $\langle$Swerve, Swerve$\rangle$ are not equilibria; in each case both parties have an incentive to defect. But the game does have a symmetric equilibria. It is that both players play the mixed strategy of Stay with probability 0.01, and Swerve with probability 0.99.

Let **Swerve** be the proposition that a rational player of kind $K$ with evidence $E$ will Swerve. And call the players Row and Column. Given our assumptions so far, plus Uniqueness, we can prove that Row's credence in **Swerve** is 0.99. Here's the proof.

1.  Let $x$ be Row's credence in **Swerve**.
2.  By self-awareness, Row knows that $x$ is her credence in **Swerve**.
3.  Since she knows she is rational, Row can infer that $x$ is a rational credence in **Swerve**.
4.  Since she knows Uniqueness is true, Row can infer that $x$ is the only rational credence in **Swerve**.
5.  Since she knows Column is rational, she can infer that $x$ is Column's credence in **Swerve**.
6.  Since all the assumptions so far are common knowledge, she can infer that Column knows that $x$ is her credence in **Swerve**.
7.  If $x = 1$, then Row can infer that it is rational for Column to Swerve, while knowing that Row will also Swerve. But this is impossible, since if Column knows Row will Swerve, it is best to Stay. So $x \neq 1$.
8.  If $x = 0$, then Row can infer that it is rational for Column to Stay, while knowing that Row will also Stay. But this is impossible, since if Column knows Row will Stay, it is best to Swerve. So $x \neq 0$.
9.  So $0 < x < 1$.
10. Since Row knows Column's credence that Row will Swerve (whatever it is), and Row knows Column is rational, but Row does not know what Column will do, it must be that Column is indifferent between Stay and Swerve given her credences about what Row will do.
11. Column is indifferent between Stay and Swerve only if her credence that Row will Swerve is 0.99. (This is a reasonably simple bit of algebra to prove.)
12. So from 10 and 11, Column's credence that Row will Swerve is 0.99.
13. By (known) Uniqueness, it follows that the only rational credence in **Swerve** is 0.99.
14. So since Row is rational, it follows that $x = 0.99$.

Now there is nothing inconsistent in this reasoning. In a sense, it is purely textbook reasoning. But the conclusion is deeply puzzling. We've proven that Column is indifferent between her two options. And we've proven that Row knows this. But we've also proven that Row thinks it is 99 times more likely that Column will choose one of the options over the other. Why is that? It isn't because there is more reason to do one than the other; given Column's attitudes, the options are equally balanced. It is purely because Uniqueness pushes us to a symmetric equilibrium, and this is the only symmetric equilibrium.

I don't think this result will convince many devotees of Uniqueness to give up their view. It's not a particularly novel claim that rational players will end up at the unique Nash equilibrium of a game. And to be sure, if this game was being played repeatedly, much weaker assumptions entail that each player should Stay 1% of the time, with those Stays being randomly distributed across the plays of the game. But it is still odd, at least to me, to see the same conclusion drawn in the single shot game, where each player is known to be indifferent between their choices.

The next case is I think much worse for Uniqueness.

## Elections

The cases that really inspired this chapter come from some recent work on this rather old question,

> If a symmetric game has an equilibrium, does it have a symmetric equilibrium?

Over the years, a positive answer was given to various restricted forms of that question. Most importantly, John @Nash1951 showed that if each player has finitely many moves available, then the game does have a symmetric equilibrium.

But recently it has been proven that the answer to the general question is no. Mark @Fey2012 showed that there are symmetric positive-sum two-player games that have only asymmetric equilibria.[^demons-and-decisions-18] And Dimitrios @Xefteris2015 showed that there is a symmetric three-player zero-sum that has only asymmetric equilibria. In fact, he showed that a very familiar game, a version of a Hotelling--Downs model of elections, has this property. Here's how he describes the game.

[^demons-and-decisions-18]: Fey also includes a nice chronology of some of the proofs of positive answers to restricted forms of the question.

> Consider a unit mass of voters. Each voter is characterized by her ideal policy. We assume that the ideal policies of the voters are uniformly distributed in \[0, 1\]. We moreover assume that three candidates A, B and C compete for a single office. Each candidate $J \in \{A, B, C\}$ announces a policy $s_J \in [0, 1]$ and each voter votes for the candidate who announced the policy platform which is nearest to her ideal policy. If a voter is indifferent between two or among all three candidates she evenly splits her vote between/among them. A candidate $J \in \{A, B, C\}$ gets a payoff equal to one if she receives a vote-share strictly larger than the vote-share of each of the two other candidates. If two candidates tie in the first place each gets a payoff equal to one half. If all three candidates receive the same vote-shares then each gets a payoff equal to one third. In all other cases a candidate gets a payoff equal to zero. [@Xefteris2015, 124]

It is clear that there is no symmetric pure-strategy equilibrium here. If all candidates announced the same policy, everyone would get a payoff of $\frac{1}{3}$. But no matter what that strategy is, if B and C announce the same policy, then A has a winning move available.

What's more surprising, and what Xefteris proves, is that there is no symmetric mixed strategy equilibria either. Again, in such an equilibrium, any player would have a payoff of $\frac{1}{3}$. Very roughly, the proof that no such equilibrium exists is that random deviations from the equilibrium are as likely to lead to winning as losing, so they have a payoff of roughly $\frac{1}{2}$. So there is no incentive to stay in equilibrium. So no symmetric equilibrium exists.

But if Uniqueness is true, and if it is possible to play the game under circumstances of common knowledge of rationality and kind, then there must be a symmetric equilibrium. The reason is that a version of the proof of the previous section still goes through. Whatever credal distribution A has over B's possible policies, A must also have over C's policies (since they both adopt the uniquely rational strategy), and she must know that B and C each have over each other's policies and over hers, and these distributions must be consistent with each player having these credal distributions while thinking that the other players have the same distributions and are maximising expected utility. In other words, the assumptions we've made about the game imply that A has a credal distribution $F$ over B's possible policies only if the mixed strategy triple where each player adopts $F$ as their mixed strategy is itself an equilibrium. And that would be a symmetric equilibrium. But no symmetric equilibrium exists.

But if we drop Uniqueness, it is possible to keep all the other assumptions. As Xefteris points out, the game has asymmetric equilibria. Here is one possible model for the game.

-   A plays 0.6 (and wins), B and C each play 0.4 (and lose).
-   Each player has a correct belief about what the other players will play.
-   But both B and C know they cannot win given the other player's moves, so they pick a move completely arbitrarily.
-   Further, each player has a correct belief about why each player makes the move they make.

This is the coherent equilibria that Xefteris describes, but note that it is rather implausible that we'd end up there in a real-life version of the game. It requires two of the players to know that one of the other players will be indifferent between their options, but from this draw a correct inference about what they will do. That's not particularly plausible. So let's note that there is a somewhat more plausible way to get all three players to make those moves.

-   A plays 0.6 (and wins), B and C each play 0.4 (and lose).
-   The only two rational plays are 0.4 and 0.6, but each of them is permissible.
-   In any world that a player believes to be actual, or a player believes another player believes to be actual, or a player believes another player believes another player believes to be actual, etc., the following two conditions hold.
-   If a player plays 0.6, they believe the other two players will play 0.4, and hence playing 0.6 is a winning move.
-   If a player plays 0.4, they believe the other two players will play 0.6, and hence playing 0.4 is a winning move.

The main difference between this model and Xefteris's is that it allows that players have false beliefs. But why shouldn't they have false beliefs? All they know is that the other players are rational, and rationality (we're assuming) does not settle a unique verdict for what players will do.[^demons-and-decisions-19] So I think this strategy set, where the players have rational (but false) beliefs about the other players, is more useful to think about.

[^demons-and-decisions-19]: To use the game-theory jargon, Xefteris describes a Nash equilibrium of the game, but what I've described is a a rationalizable strategy triple [@Bernheim1984, Pearce1984]. If Uniqueness is true, then strictly speaking any rationalizable strategy pair for a symmetric game is a Nash equilibrium.

And what it shows, I think, is that Uniqueness implies a false claim about which games are possible. Given Uniqueness, it is impossible for players who know each other to be rational to play this game. But it is possible for players who know each other to be rational to play this game; we've just seen a way the world could be where both players know the other to be rational. So Uniqueness is false - it implies the impossibility of something we've seen to be possible.

## Objections

The reductio arguments in the previous two sections have assumed not just that Uniqueness is true, but that the players know that it is true. What happens if we drop that assumption, and consider the possibility that Uniqueness is true but unknowable?

This possibility is a little uncomfortable for philosophical defenders of Uniqueness. If the players in these games do not know that Uniqueness is true, then neither do the authors writing about Uniqueness. And now we have to worry about whether it is permissible to assert in print that Uniqueness is true. I wouldn't make too much of this though. It is unlikely that a knowledge norm governs assertion in philosophical journals.

The bigger worry here is that one key argument for Uniqueness seems to require that Uniqueness is knowable. A number of recent authors have argued that Uniqueness best explains our practice of deferring to rational people.[^demons-and-decisions-20] For instance, Greco and Hedden use this principle in their argument for Uniqueness.

[^demons-and-decisions-20]: There is a nice discussion of this argument, including citations of the papers I'm about to discuss, in @KopecTitelbaum2016 [195].

> If agent $S_1$ judges that $S_2$'s belief that $P$ is rational and that $S_1$ does not have relevant evidence that $S_2$ lacks, then $S_1$ defers to $S_2$'s belief that $P$. [@GrecoHedden2016, 373].

Similar kinds of arguments are made by @Dogramaci2012 and @Horowitz2014. But the principle looks rather dubious in the case of these games. Imagine that A forms a belief that B believes that a rational thing to do in the Xefteris game is to play 0.6, and so she will play 0.6. She should judge that belief to be rational; as we saw it is fully defensible. But although she does not believe that she has evidence that B lacks, she should not defer to it. At least, she should not act as if she defers to it; believing that B will play 0.6 is a reason to play something other than 0.6.

And that's the general case for these symmetric games with only asymmetric equilibria. Believing that someone else is at an equilibrium point is a reason to not copy them. If it were not a reason to not copy them, then the strategy profile where each player plays the same thing would be a symmetric equilibrium. So thinking about these games doesn't just give us a rebutting defeater for Uniqueness, as described in the previous two sections, but an undercutting defeater, since they also tell against a premise that has been central to recent defences of Uniqueness.

I think there is a somewhat better move available to the Uniqueness theorist. They could simply deny that the Xefteris game, as I've described it, is even possible.[^demons-and-decisions-21] This perhaps isn't as surprising as it might seem.

[^demons-and-decisions-21]: This is really just a response to the argument based on that game; I think they just have to say that in Chicken a rational player will rationally think the other player is more likely to make one of the two choices with equal expected payoffs.

Note two things about the Xefteris game. First, it is an infinite game in the sense that each player has infinitely many choices. It turns out this matters to the proof that there is no symmetric equilibrium to the game. Second, we are assuming it is common knowledge, and hence true, that the players are perfectly rational. Third, we are assuming that perfect rationality entails that people will not choose one option when there is a better option available. When you put those three things together, some things that do not look obviously inconsistent turn out to be impossible. Here's one example of that.

> A and B are playing a game. Each picks a real number in the open interval (0, 1). They each receive a payoff equal to the average of the two numbers picked.

For any number that either player picks, there is a better option available. It is always better to pick $\frac{x+1}{2}$ than $x$, for example. So it is impossible that each player knows the other is rational, and that rationality means never picking one option when a better option is available.

So the Uniqueness theorist could say that the same thing is going on in the Xefteris game. Some infinitely games cannot be played by rational actors (understood as people who never choose sub-optimal options); this is one of them. But if this is all the Uniqueness theorist says, it is not a well motivated response. We can say why it is impossible to rationally play games like the open interval game; the options get better without end. But that isn't true in the Xefteris game. The only thing that makes the game seem impossible is the Uniqueness assumption. People who reject Uniqueness can easily describe how the Xefteris game can be played by rational players. Simply saying that it is impossible, without any motivation or explanation for this other than Uniqueness itself, feels like an implausible move.

## Conclusion

If Uniqueness is true, then the following thing happens in games between people who know each other to be the same kind, and to be rational. When someone forms a belief about what the other person will do, they can infer that this is a rational way to play the game given knowledge that everyone else will do the same thing. But sometimes this is a very unintuitive inference. In Chicken, it implies that we should have asymmetric attitudes to someone who is facing a choice between two options with equal expected value. In the election game Xefteris describes, a game that feels consistent turns out to be impossible.

I think the conclusion to draw from these cases of symmetric interactions this is that Uniqueness is false, and hence permissivism is true. Sometimes in such an interaction one simply has to form a belief about the other player, knowing they may well form a different belief about you. Indeed, sometimes only coherent way to form a belief about the other player is to believe that they will form a different belief about you. And that means giving up on Uniqueness. And if we give up on Uniqueness, then there is no tension between the views in chapters \@ref(decisive) and \@ref(coherence).

# Puzzles about Weak Dominance

The account causal ratificationism gives of what makes a decision rational has two clauses. The first is that the decision has to make sense once it is made. That is, it has to maximize expected utility given some credal distribution that is rational once the decision is made. The second is that the decision must not be weakly dominated. That is, given the states and choices that are possible (given that the decision is made), no other option must have the status that it could do better, and couldn't do worse. The first clause seems very intuitive to me, and is backed up by plausible principles about choices like the ABC game. The second clause seems less intuitive. But it, or something like it, is required to make sense of those same plausible principles, and so I've included it as part of causal ratificationism.

This chapter has four aims, corresponding to its four sections. In section \@ref(whyweak) I'll show why the ABC game supports the idea that rational choosers do not select weakly dominated options. In section \@ref(boundaries) I'll note one reason that weak dominance unintuitive; it suggests that seemingly insignificant boundaries are in fact significant. If it weren't for the argument of the previous section, this would be enough to convince me to do without weak dominance. In section \@ref(redgreen) I'll discuss how weak dominance interacts with some hard puzzles about a simple symmetric game. I've had two previous attempts to say something sensible about this game [@Weatherson201x; Weatherson201y]; hopefully third time's the charm. Finally, in section \@ref(iwd), I'll argue that it can be rational to choose options which are ruled out by iterated weak dominance. The argument will primarily turn on an analysis of the money-burning game \[cite\], and I'm going to endorse Stalnaker's \[cite\] criticism of the standard treatment of that game.

## Why Weak Dominance {#whyweak}

Consider the following version of the ABC game.

```{tikz, abc-game-weak-dominance, fig.cap = "A version of the ABC game that supports weak dominance.", fig.ext = 'png', cache=TRUE, fig.width=4}
\usetikzlibrary{calc}
\begin{tikzpicture}[scale=1.5,font=\footnotesize]
\tikzset{
% Two node styles for game trees: solid and hollow
solid node/.style={circle,draw,inner sep=1.5,fill=black},
hollow node/.style={circle,draw,inner sep=1.5},
square node/.style={rectangle,draw, inner sep = 1, fill = black}
}
% Specify spacing for each level of the tree
\tikzstyle{level 1}=[level distance=20mm]
\tikzstyle{level 2}=[level distance=15mm,sibling distance=15mm]
\tikzstyle{level 3}=[level distance=15mm,sibling distance=15mm]
\tikzstyle arrowstyle=[scale=1]
\tikzstyle directed=[postaction={decorate,decoration={markings,
mark=at position .5 with {\arrow[arrowstyle]{stealth}}}}]
% The Tree
\node(0)[hollow node,label=above:{$Demon$}]{}

child[grow=left, level distance=25mm]{node(1)[square node, label=left:{$2, 2$}]{}
edge from parent node[above]{A}
}
child[grow=225]{node(3)[solid node]{}
child[grow=240]{node[square node,label=below:{$1, 1$}]{}edge from parent node[left]{U} }
child[grow=300]{node[square node,label=below:{$0, 0$}]{} edge from parent node[right]{D}}
edge from parent node[left, xshift = -3]{B}}
child[grow=315]{node(4)[solid node]{}
child[grow=240]{node[square node,label=below:{$1, 0$}]{}edge from parent node[left]{U} }
child[grow=300]{node[square node,label=below:{$0, 1$}]{} edge from parent node[right]{D}}
edge from parent node[right,xshift = 3]{C}
}
;
% information set
\draw[dashed](3) to (4);
% specify mover at 2nd information set
\node at ($(3)!.5!(4)$) [above] {$Chooser$};
\end{tikzpicture}
```

**GOTTA CLARIFY WHETHER I'M REALLY ALREADY COMMITTED TO THIS**

**ALSO RAISE WORRY ABOUT WHETHER WEAK DOMINANCE LEADS TO PARADOX IN VERSIONS OF ABC WHERE DEMON HAS PR 1 IN A**

Given what I've said already about the ABC game, I'm committed to the following two claims. First, in this game the only rational choice is $U$. That's the only rational choice at the only information set where Chooser might move, and that determines the rational strategy. Second, a choice is rational in this game iff it is rational in the following simultaneous move game.

```{r,abc-weak-dominance-strategic, cache=TRUE}
generic_abc_strategic <- tribble(
	   ~"", ~A, ~B, ~C,
	   "U", "$2, 2$", "$1, 1$", "$1, 0$",
	   "D", "$2, 2$", "$0, 0$", "$0, 1$"
	)
gameformat(generic_abc_strategic, "A strategic representation of this ABC game.")
```

From these claims, it follows that the only rational move in the game in \@(tab:abc-weak-dominance-strategic) is $U$. But that's hard for the causal ratificationist to explain, since it seems that $D$ is ratifiable. If Chooser believes, after choosing $D$, that the probability that Demon will play $A$ is 1, then playing $D$ will maximise expected utility. Is that a rational credence to have? Maybe! After all, Chooser knows that Demon is a utility maximiser, and for Demon, $A$ strictly dominates $B$ and $C$. So if causal ratificationism was just restricted to the view that choices must be ratifiable, it would not be consistent with what I've said about the ABC game, since it would say that $D$ is impermissible in the extensive form version of the game, but permissible in the strategic form.

This shows that I need to add something to causal ratificationism to make it consistent with what I've said about the ABC game. Adding weak dominance would solve this problem. While $D$ is ratifiable in the strategic form of the game, it is weakly dominated. So adding the weak dominance clause restores the key constraint that the two forms of the game are treated the same way. And that's the primary reason that I added this clause to causal ratificationism.

Note that this argument is well short of a proof that weak dominance is needed. It shows something is needed, and weak dominance would be sufficient, but it doesn't rule out weaker constraints. But I can't see any weaker constraint that would suffice to bring these two problems back into alignment, and would be nearly as intuitive as weak dominance. So I've added it as the second clause.

There is one intuitive motivation for weak dominance. The core idea behind causal ratificationism is that choices should be defensible. The chooser should be able to say "This is what I'm doing, and this is why I'm doing it," in a way that makes sense. And there is something strange about a speech defending choosing $D$ in \@(tab:abc-weak-dominance-strategic). It looks like the person who chooses $D$ is taking a risk for which they are not receiving any compensation. And it's irrational to take uncompensated risks. That intuition, if it is correct, generalises to all cases of choosing weakly dominated options, and so supports a general bar on choosing weakly dominated strategies.

Weak dominance has some problems though, and I'll turn to them next.

## The Boundaries of Games {#boundaries}

The definition of weak dominance involves quantifying over possible ways the world might be. In the cases that have been most central to this book, those states have been moves made by another agent, typically a demon. So we could say, equivalently, that the definition of weak dominance involves quantifying over moves available to the other player. Indeed, the usual definition of weak dominance in game theory does quantify over moves available to the other player. A consequence of that is that the boundary between the available and the unavailable moves becomes very significant. That's a problem, because often this boundary seems to be insignificant.

The importance of this boundary is a key difference between the two clauses in causal ratificationism. The definition of expected utility maximization also makes an appeal to the boundary between possible and impossible states. But whether one treats a state as impossible, or as possible but with probability zero, doesn't affect the expected utility of an action. The difference between treating a state as impossible, and as possible but with probability zero, does matter to determining which states are weakly dominant.

It's easier to think about this boundary with some concrete examples. So let's get away from the simple games that have been the focus of this book, and imagine that Chooser is playing chess with Demon. In this game, Chooser has the black pieces. The game starts in a fairly traditional manner: 1. e4 e5, 2. Nf3.

```{r chessboard, echo=FALSE, fig.align='center', fig.height = 5, fig.width = 5}
include_graphics("chessboard.png")
```

Now it is Chooser's turn to move, and they are thinking about Qh4. This is obviously a bad move, but I want us to think for a minute about why it is a bad move.

The causal ratificationist says that when evaluating a move, one thing to check is whether it is weakly dominated. To do that, one must know what the possible states are, i.e., what the possible moves for Demon are. Now pretty clearly Qh4 is not going to be utility maximizing, so the answer to this question won't affect what the causal ratificationist ultimately says about whether one should play Qh4. But it is important to figure out just what the theory says, so it is important to figure out just what these 'possible moves' are.

In particular, which of the following two 'moves' should be in the domain of quantification when we are asking whether a choice by Chooser is weakly dominated?

1.  Demon moves Ng5.
2.  Demon knocks over the board and walks away.

There is an important sense in which 1 is in the domain, and 2 is not. The rules of chess allow for Ng5, and they do not allow for knocking the board over. When I appealed to weak dominance earlier, I assumed that all and only the moves specified in the rules of the game went into the domain of quantification, and I suspect most readers went along with that. The same principle here would include Ng5, and exclude knocking the board over, as possible moves.

This division is hard to motivate from a traditional philosophical perspective. There is no relevant epistemic difference between the two options. Given very weak assumptions, Chooser knows that Demon will take neither of these options. There is no relevant difference between the options in terms of ability. Demon can move Ng5, and can also (given some assumptions about Demon's corporal form) knock the board over. The difference between the options is essentially a legal one. Ng5 is a legal move, and knocking the board over is not. That matters when one is playing a formal game. But game theory isn't just meant to be about formal games like chess. It is meant to include human interactions, notably including wars, that don't have these kinds of clear rules. And in those settings, it is unclear what counts as a possible move, and hence unclear what counts as a weakly dominant move.

Let's bring this all back to decision theory involving demons. Chooser is playing the following game. They have to choose $U$ or $D$; if they choose something else, they get nothing. Demon is very good at predicting them, and will either turn to the left, if they predict $U$, or turn to the right, if they predict $D$. The turn will be after Chooser writes down their choice, but before it is revealed. If Chooser writes $U$, they get 1 whichever way Demon chooses, but if Chooser writes $D$, they get 1 if Demon turns right, and 0 if Demon turns left. So far, this looks like a case where weak dominance matters. The expected return of each move is 1, but $U$ weakly dominates $D$. Let's add something to the game though. Demon will turn one way or the other; Chooser knows that. But if Demon instead drinks a beer, Chooser will get 1 if they have chosen $D$, and 0 if they have chosen $U$. Demon won't drink a beer; Demon is allergic to beer, and there isn't any beer around. But these are the payoffs if beer drinking happens. Which it won't. Question: Does $U$ still weakly dominate $D$? And this suggests a further question: Is $D$ permissible?

The problem for using weak dominance in a theory of choice, and it's a problem wherever weak dominance is used, not just in causal ratificationism, is this. Given that Demon definitely won't drink beer, it seems like it shouldn't matter whether we exclude beer from the decision table, or include it while marking it as maximally unlikely to happen. But given that weak dominance is one of our constraints, it turns out this makes all the difference in the world. That seems unfortunate.

Here is how I think we should deal with this problem. I've said *deal with* rather than *solve* advisedly. It is a problem; it's why I'm not happy having to include a weak dominance clause. But there are ways to deal with it.

In the first instance, causal ratificationism is a theory of decision not in real world decision problems, but in the kind of formalized problems we discuss in decision theory and game theory textbooks. Solving a real world problem is a two-step process. First, the real world problem has to be translated into a formal problem. Causal ratificationism has something to say about this---it says that the states must be causally independent of the options---but it doesn't have a lot to say. Second, the formal problem has to be solved. Causal ratificationism has a lot to say about this stage, and I've been spelling out what it has to say at some length in this book.

Usually, there are multiple acceptable ways to translate a real world problem into a formal problem. Sometimes, the different translations will have different solutions. In those cases, it is indeterminate what is rational to do. Arguably, that's the case in the case I described a few paragraphs back, about turning and beer-drinking. It's acceptable to model this as a problem where Demon's only options are to turn left or to turn right. On that model, the only rational move is $U$. It's also acceptable to model it as a problem where Demon could (but won't) drink beer. On that model, both $U$ and $D$ are rationally permissible. So it is determinately true that $U$ is rationally permissible, but indeterminate whether $D$ is rationally permissible. That is a plausible enough response to the problem, I think.

I said earlier that to solve a real world problem, one has to do two things: translate the problem into a formal problem, and solve the formal problem. There is a way that might be misleading. It might suggest that the two steps are analytically distinct from each other, and that the plausibility of an answer to one problem should be assessed independently of the answer to the other. That's not right. Whether a formal model of a real world problem is acceptable is a function, in part, of whether the solutions to the formal problem are reasonable solutions to the real world problem. It's perfectly sensible to reason as follows. If it is clear that $D$ is an unreasonable move in the real world game (I don't think this is clear, but someone might), then it follows that modeling the game as a formal game where Demon has three possible moves---turn left, turn right, drink beer---is to model the game incorrectly. There is no such thing as an absolutely correct or incorrect model of a game, or of any other real world situation; the quality of a model is dependent on what the model is being used for. And this also helps us avoid some of the most troublesome consequences of the fact that weak dominance is dependent on just how a game is modeled. Sometimes, the fact that rational choices are not weakly dominated is evidence for or against modeling the game a particular way.

## Three Kinds of Demon {#redgreen}

Set up red green game and properly cite

Note this is my third try at it

First demon, limit prob. This one is easy

Second demon, zero prob but possible. Answer one this can't happen. But this takes us into infinitesimal territory and I'm not going there. Answer two, the speech sounds bad. Don't take uncompensated risks.

Third demon, can not fail. Then not in fact weak dominance post choice bc alternative is not in fact possible.

Same goes for symmetric humans but third is really impossible.

## Benefits of Weak Dominance

Chooser and Demon pick an integer between 0 and 1,000,000.

Demon gets 1 if same, 0 if otherwise, and Demon is very good

Chooser gets lower of two numbers unless they pick the same positive number, in which case Chooser gets that minus two.

Only ratifiable outcome is 0,0

But that's weakly dominated by just about everything else, and so should be rejected.

Weak dominance helps here.

Question: What equilibria are there? Feels like there should be a mixed strategy one.

------------------------------------------------------------------------

Note to self: Think about case where game is capped at 10. Is there an equilibrium where Chooser plays 1/2 9, 1/2 10.

Need expected return of 9 and 10 to be the same.

Demon plays 9 with prob x, 10 with prob 1-x.

If Chooser plays 9, gets 7x + 10(1-x) = 10 - 3x.

If Chooser plays 10, gets 9x + 8(1-x) = 8 + x

Haha, it's 50/50.

------------------------------------------------------------------------

Another sort of benefit. Think about the following three option case.

PU PM PD U 0 5 2 M 5 0 2 D 2 2 2

Is D acceptable? No, I say, it's weakly dominated by 1/2 U, 1/2 M. That's good enough. Ideally, Chooser would play that. Even if they can't play it, they should.

The same helps if you take the earlier problem, and say that if 0, 0, Chooser gets payout epsilon. It's still weakly dominated by this mixture. Oh that's not right - Hmm come back to this.

## Iterated Weak Dominance {#iwd}

Sometimes thought that if WD then committed to IWD. Cite HH and Stalnaker in reply.

Three objections 1. Order effects 2. Gets unintuitive results 3. Nothing incoherent about speeches that violate

Start with Bonanno on order effects

Do strategic version of money burning and show what IWD leads to Note that nothing wrong with all H speech This might get complicated

Now do dynamic version Really absurd that having an untaken option can make a difference, when others know you won't take it But does HH mean that you think demon is stupid? No it means that you think demon might follow up stupid with stupid Also do this using counterfactuals maybe

# (APPENDIX) Appendicies {.unnumbered}

# Game Theory {#gametheory}

This appendix is a brief introduction to some important game theoretic concepts. It won't introduce anything new to anyone who has studied practically any game theory before. But since that's not true of all philosophers, I thought it was helpful to briefly introduce the basics here.

# About a Demon {#aboutademon}

Include stuff about arbitrarily accurate Note that known perfect accuracy leads to challenges, one's we'll come back to in...

Also note that we're assuming the demon predicts the strategy a player chooses. That could include randomisation.

Note that I'm simply assuming expected utility theory is right for non-demonic problems.

But that does imply an assumption that there is some reason for that. And I think that's got to include something like a Sure Thing Principle.

But I am including the open box Newcomb Problem as demonic.

And maybe a riff on why it's Player/Chooser/Agent whatever

# Methodology {#methodology}

There are three primary sources of evidence that are available in constructing and evaluating decision theories: principles, sameness of cases, and cases. Most of the literature focusses on the third of these. I think that's a mistake; this is by far the least reliable source of evidence. We should instead focus on the first two. That's what I'll do in this book, and the purpose of this section is to defend this methodological starting point.

By principles, I mean claims like *Preferences should be transitive*, or *Choosers should not regret their choices as soon as they are made*. These are very general claims about what the structure of one's choices should look like. There aren't a lot of principles like these that we can be antecedently very confident in. But there are some, and those we should hold on to barring extraordinary evidence.

By sameness of cases, I mean claims that two particular decisions should get (in some sense), the same choice. Here is a familiar example from a non-Demonic decision problem. Chooser has a ticket to today's cricket match, and is deciding whether to go. Chooser enjoys watching cricket, but does not enjoy sitting around in the stands waiting for the rain to clear, and there is a good chance of rain. What should Chooser do? Well, we haven't said nearly enough to settle that, so let's ask something more precise. What more do we need to know to know what Chooser should do? We do need to know how much Chooser likes watching cricket, dislikes sitting around in the rain, will have to pay to get to the ground and how likely rain is. But we don't need to know how much Chooser paid for the ticket. That's a sunk cost. If we settle all the forward looking parameters (likelihood of rain, utility of going under different scenarios, etc), then changing the backwards looking ones (how much Chooser paid) doesn't make a difference. If it would be rational to stay home given all those parameters and having paid \$10 for the ticket, it would be rational to stay home given all those parameters and having paid \$100 for the ticket. Even in hard cases, and if the weather is bad enough Chooser may have a very hard choice here, we often have clear enough knowledge that some differences do not in fact make a difference.

And by cases I mean claims about what Chooser should do in a particular vignette. These are the main form of evidence that get used in philosophical decision theory. We (the theorists) are told that Chooser faces a problem like the one in \@ref(tab:basic-stag), with a Demon predicting the choice and the payout dependent on the actions of the two of them.

```{r, basic-stag, cache=TRUE}
basic_stag <- tribble(
	   ~"", ~PA, ~PB,
	   "A", "$6$", "$0$",
	   "B", "$4$", "$3$"
	)
gameformat(basic_stag, "An example of a choice situation")
```

A lot of philosophers seem to the following approach to decision theory. (I say 'seem to' because I've never really seen a good defence of this methodology, but I have seen a lot of arguments from cases like this to sweeping theoretical claims.) First, we figure out what Chooser should do in this case, perhaps by consulting our intuitions. Second, we work out which theory best fits with what we've learned this way about individual cases.

Now it would be wrong to say that the three kinds of evidence I've presented here fall into three very neat categories. The boundaries between them are blurry at best. Any evidence about an individual case can be turned into a kind of principle by simply replacing the names in the vignette with variables and universally quantifying over the variables. We'll get very restricted principles that way---anyone facing just that game should play B, for example---but all principles have some restrictions on them. And there's not much distance between judging that what Chooser should do is independent of what they paid for the ticket, and judging that the principle *Sunk costs are irrelevant* is part of our evidence. But the fact that a boundary isn't sharp doesn't mean that it's theoretically useless. The paradigms of the three kinds of evidence are different enough that we can helpfully keep them in mind when understanding what particular theorists are doing.

And the paradigm that starts with individual cases, like the table I just presented, seems considerably worse than the other two paradigms. Arguments in that family are vulnerable to two kinds of objection that arguments that start with the other kinds of evidence are not.

First, judgments about cases might conflate what the standards of ideal rationality say about the case with what the standards of non-ideal rationality say about the case. Consider this example, which I discuss more in chapter \@ref(dilemma).

> **Quick Basketball Choice**\
> Chooser has to name an NBA team. (The NBA is the main club basketball competition in the world.) If the team wins the NBA championship this year, Chooser wins a million dollars. But if Chooser thinks about any basketball player before naming the team, Chooser will be cast into the fires of Hell for all of eternity. What should Chooser do?

I think Chooser should say the first team name that comes into their head, or just pass and get out of the game if that's possible. They should very much not try to make the choice that maximises expected utility. That will require forming probabilistic judgments about the likelihood of different teams winning, and that will involve thinking about the players on the team, and that will involve damnation. Just say something and hope for the best.

Is this a counterexample to the claim that in non-Demonic cases Chooser should maximise expected utility? No, because that claim is about ideal rationality. When there are constraints on how Chooser can make the choice, ideal rationality doesn't apply. Chooser should be judged by standards of non-ideal rationality, and non-ideal rationality says to say the first team name that comes into his head. I'll make all this a fair bit more precise in chapter \@ref(dilemma), including coming back to whether it makes sense to talk as if there is a single standard of non-ideal rationality. But I hope it's reasonably clear that something like this is the right thing to say. There are multiple ways we can judge individual choices in individual cases. The standards of ideal rationality are one of those ways to judge cases, but not the only way. And sometimes when we ask what someone should do, we are (implicitly) using one of those other ways. I think most purported counterexamples to one or other decision theory in the literature are no more convincing than using **Quick Basketball Choice** to refute expected utility theory. This is especially true when they involve, as this case does, constraints not just on what decision is made, but how it is made. (For example, when they involve the decision maker being punished for randomising). And this is one reason why I distrust arguments from particular examples.

The second reason is more internal to decision theory. The project I'm engaged in, and the project that most philosophers who write about demonic decision theory are engaged in, is the project of generalising expected utility theory to cover Demonic cases. But the methodology of starting with judgments about individual decisions and finding the theory that best fits them does not, in fact, lead to expected utility theory. So that methodology is inconsistent with the theory we are assuming to be correct in non-Demonic cases. So there is a fairly deep tension in any work that tries to generalise expected utility theory by appeal to intuitions about cases. This point requires a bit of background to make, so let's come back to it after saying more precisely what the theory that we're all trying to generalise is.

# Decision Tables and Decision Problems {#tables}

Five questions

1.  What is a permissible state?
2.  When is it permissible to end the list of states?
3.  What is a permissible option?
4.  When is it permissible to end the list of options?
5.  What do the values measure?

And a sixth question, what does it take to individuate a problem.

# Non-Demonic Decision Theory {#quiggin}

# Backward Induction {#backwardinduction}

What I'm using Joyce's review of Ahmed for why it matters The strong version I'm not using (or maybe that I am using) Stalnaker on why it shouldn't be used
