# identifying_inventory_issues

There are several databses at work that have problems with repeated inventory numbers (these are supposed to be UIDs) on pieces of equipment

Sometimes, the pairs of inventory numbers refer to the same object, other times, different objects, other times there are typos.

Essentially, I was told that there are lots of problems with the databases. We want to
- Identify the items that need to be reassigned to UIDs
- Delete the truly repeated item enteries (correcting for typos, snytax convolutions, synonyms, etc)
- Identify the shortcomings in business logic
- Clean each database individually, then clean duplicates across databases

Overall, we are dealing with 60K+ items logged

Attached code divides the duplicated enteries into 5 categories based off matches on different business criteria
