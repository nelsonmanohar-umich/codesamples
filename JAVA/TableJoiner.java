import java.lang.*;
import java.util.*;
import java.io.*;


/*
 * Performs a join as specified between the contents of two files
 * each representing separator (e.g., comma) delimited tables 
 * (e.g., csv files).
 * <p>
 * @author  Nelson Manohar
 */
public class TableJoiner {
    public Integer debug = 0;
    private HashMap<String, String> Options = new HashMap<String, String>();  
    private HashMap<String, List<Integer>> SmallTableIndex = new HashMap<String, List<Integer>>();  
    private HashMap<Integer, String> SmallTableLines = new HashMap<Integer, String>();  
    private HashMap<Integer, Integer> SmallTableVisited = new HashMap<Integer, Integer>();  
    private TableJoinData JoinData = new TableJoinData();

    public void TableJoiner() {
        return;
    }

    private static class TableJoinData {
        private String query;
        private String jointype = "";
        private String large_table;
        private String small_table;
        private Integer large_table_fieldnum = 0;
        private Integer small_table_fieldnum = 0;
        private String header = ""; 
        private Integer has_header = 1;
        private Integer debug = 0;
        private String small_query_field;
        private String large_query_field;
        private String separator = ",";
        private String left_empty = "";
        private String right_empty = "";
    
        private void TableJoinData(Integer debug) {
            this.debug = debug;
            return;
        }

        private void initialTableSetup(String large_table, String small_table, Integer large_table_fieldnum, Integer small_table_fieldnum, String jointype, String separator) {
            this.large_table = large_table;
            this.small_table = small_table;
            this.large_table_fieldnum = large_table_fieldnum;
            this.small_table_fieldnum = small_table_fieldnum;
            this.jointype = jointype.toUpperCase();
            this.separator = separator;
            return;
        }

        private void firstTableSetup(String line) {
            header = line;
            String[] words = splitLine( line );
            String current_key = words[small_table_fieldnum]; 
            if ( jointype.equals("RIGHT") || jointype.equals("FULL") )
                for ( int i =0; i<words.length; i++ ) 
                    left_empty = left_empty + separator;
            small_query_field = String.format("%s.%s", small_table, current_key);
            return;
        }

        private void secondaryTableSetup(String line) {
            String[] words = splitLine( line );
            if ( jointype.equals("LEFT") || jointype.equals("FULL") )
                for ( int i =0; i<words.length; i++ ) 
                    right_empty = right_empty + separator;
            String current_key = words[large_table_fieldnum]; 
            large_query_field = String.format("%s.%s", large_table, current_key);
            header = String.format("%s%s%s", line, separator, header);
            return;
        }

        private void printJoinTableHeader() {
            if ( has_header == 1 )
                System.out.println( header );
            return;
        }

        // make options into hash dictionary and iterate over keys/options
        private void printState() {
            String data;
            query = String.format(">>>JOIN OUTPUT FOR: %s %s JOIN %s ON (%s = %s)", large_table, jointype, small_table, this.large_query_field, this.small_query_field);
            System.out.println( "----------------------------------------------------------" );
            System.out.println( query );
            if ( debug > 0 ) {
                System.out.println(String.format(">>> %14s: %s ", "debug", debug));
                System.out.println(String.format(">>> %14s: %s ", "left_empty", left_empty ));
                System.out.println(String.format(">>> %14s: %s ", "right_empty", right_empty ));
                System.out.println(String.format(">>> %14s: %s ", "separator", separator));
            }
            System.out.println( "----------------------------------------------------------" );
            return;
        }

        public String[] splitLine( String line ) {
            line = line.trim();
            String[] words;
            if ( separator.equals("|") )
                words = line.split("\\|"); 
            else
                words = line.split(separator);
            return words;
        }
    }


    public void setup() {
        String large_table = Options.get("large_table");
        String small_table = Options.get("small_table");;
        Integer large_table_fieldnum = Integer.parseInt(Options.get("large_table_fieldnum"));
        Integer small_table_fieldnum = Integer.parseInt(Options.get("large_table_fieldnum"));
        String jointype = Options.get("jointype");
        String separator = Options.get("separator");
        if ( debug > 0 ) {
            System.out.println( "----------------------------------------------------------" );
            System.out.println( ">>>    large csvfile specified, defaulting to  : " + large_table );
            System.out.println( ">>>    small csvfile specified, defaulting  to : " + small_table );
            System.out.println( ">>>    column num. to join on first table : " + large_table_fieldnum );
            System.out.println( ">>>    column num. to join on second table: " + small_table_fieldnum );
            System.out.println( ">>>    join-type specified for the merge, using: " + jointype + " JOIN" );
            System.out.println( ">>>    join-type specified for the merge, using: " + separator );
            System.out.println( "----------------------------------------------------------" );
        }
        JoinData.initialTableSetup(large_table, small_table, large_table_fieldnum, small_table_fieldnum, jointype, separator);
        return;
    }


    public void buildIndex() {
    /*
     * Builds an index of the entries found on the column pointed to by fieldnum
     * wrt to the line numbers of the file where an entry (representing a key) 
     * is found. These entries represent keys to be used by the join.
     * @params   small_table_fieldnum    the column number to be indexed on the small table
     * @see      SmallTableLines         a cache of each line on small table is populated
     * @see      SmallTableIndex         an index mapping keys to lines
     * @returns  void
     */
        String line;
        int nline = 0;
   
        try {
            BufferedReader fp = new BufferedReader( new FileReader(JoinData.small_table));
            while ( (line = fp.readLine()) != null ) {
                nline++;
                if (nline == 1) { 
                    JoinData.firstTableSetup(line);
                    if ( JoinData.has_header == 1 )
                        continue;
                }
                SmallTableLines.put( nline, line );
                SmallTableVisited.put( nline, 0 );
                String[] words = JoinData.splitLine( line );
                String current_key = words[JoinData.small_table_fieldnum]; 
                
                List<Integer> lineset = SmallTableIndex.get(current_key); 
                if ( lineset != null ) {
                    lineset.add(nline);
                    SmallTableIndex.put( current_key, lineset );
                } else {
                    List<Integer> new_lineset = new ArrayList<Integer>( new HashSet<Integer>());
                    new_lineset.add(nline);
                    SmallTableIndex.put( current_key, new_lineset );
                }
            }
        }
        catch ( FileNotFoundException e ) {
            System.out.println( "----------------------------------------------------------" );
            System.out.println( "Expecting datafile to be named: " + JoinData.small_table );
            System.out.println( e.getMessage() );
            System.exit(-2);
        }
        catch ( IOException e ) {
            System.out.println( "----------------------------------------------------------" );
            System.out.println( "Exception while reading data from file" );
            System.out.println( e.getMessage() );
            System.exit(-3);
        }
        catch ( Exception e ) {
            System.out.println( "----------------------------------------------------------" );
            System.out.println( "An unexpected exception took place" );
            System.out.println( e.getMessage() );
            System.exit(-1);
        }
    
        List<String> old_keyset = new ArrayList<String>(SmallTableIndex.keySet());
        for ( String key : old_keyset ) {
            HashSet<Integer> lineset = new HashSet<Integer>(SmallTableIndex.get(key));
            List<Integer> lineset2 = new ArrayList<Integer>(lineset);
            SmallTableIndex.put( key, lineset2 );
        }
        return;
    }

    
    public void printIndex() {
        System.out.println( "----------------------------------------------------------" );
        List<String> keyset = new ArrayList<String>(SmallTableIndex.keySet());
        Collections.sort( keyset );
        for ( String key : keyset ) {
            System.out.print( "KEY [" + key + "] ==>  FOUND ON LINES: [" );
            List<Integer> lineset = new ArrayList<Integer>( SmallTableIndex.get(key));
            Collections.sort(lineset );
            for ( Integer lineno : lineset ) {
                 System.out.print( lineno + " ");
            }
            System.out.println( "]" );
        }
        System.out.println( "----------------------------------------------------------" );
        return;
    }

    
    public void joinTables() {
        String line;
        int nline = 0;
   
        if ( SmallTableIndex == null ) {
            System.out.println( "----------------------------------------------------------" );
            System.out.println( "MUST BUILD INDEX FIRST" );
            System.out.println( "----------------------------------------------------------" );
            this.buildIndex();
            System.out.println( "----------------------------------------------------------" );
        }

        try {
            BufferedReader fp = new BufferedReader( new FileReader(JoinData.large_table));
            String previous_key = "";
            Integer duplicate_start = 0;
            while ( (line = fp.readLine()) != null ) {
                nline++;
                int j = 0;
                if (nline == 1) {
                    JoinData.secondaryTableSetup(line);
                    JoinData.printState();
                    JoinData.printJoinTableHeader();
                    if ( JoinData.has_header == 1 )
                        continue;
                }
                if ( debug > 1 ) System.out.println( nline + "= " + line );

                String[] words = JoinData.splitLine( line );
                String current_key = words[JoinData.large_table_fieldnum]; 

                if ( debug > 1 ) {
                    if ( current_key.toString().equals(previous_key) ) {
                        System.out.println("FIELD HAS DUPLICATE KEYS, MERGE DEGENERATES TO CARTESIAN PRODUCT FOR ITEMS BELOW");
                        duplicate_start = 1;
                    } else {
                        if ( duplicate_start == 1 ) {
                            System.out.println("FIELD HAD DUPLICATE KEYS, MERGE DEGENERATED TO CARTESIAN PRODUCT FOR ITEMS ABOVE");
                            duplicate_start = 0;
                        }
                    }
                }

                List<Integer> lineset = SmallTableIndex.get(current_key);
                if ( lineset == null ) {
                    switch( JoinData.jointype ) {
                        case "INNER": break;
                        case "LEFT":  if ( debug > 0 ) System.out.print("L:");
                                      System.out.println( line + JoinData.right_empty );
                                      break;
                        case "FULL":  if ( debug > 0 ) System.out.print("FL:");
                                      System.out.println( line + JoinData.right_empty );
                                      break;
                        case "RIGHT": break;
                    }
                    continue;
                }
                if ( debug > 0 ) System.out.println( current_key + ": " + lineset );

                previous_key = current_key.toString();

                Collections.sort(lineset );
                for ( Integer lineno : lineset ) {
                    String matching_line = SmallTableLines.get( lineno );
                    if ( matching_line != null ) {
                        SmallTableVisited.put( lineno, 1 );
                        if ( debug > 0 ) {
                            j++;
                            System.out.println( nline + ": " + j + "= " + line + "<---|--->" + matching_line ); 
                        } else 
                            System.out.println( line + JoinData.separator + matching_line ); 
                    }
                }
            }

            for ( Integer k : SmallTableVisited.keySet()) {
                int visited = SmallTableVisited.get(k);
                String matching_line = SmallTableLines.get( k );
                if ( visited == 0  ) {
                    switch( JoinData.jointype ) {
                        case "INNER": return;
                        case "LEFT" : return;
                        case "RIGHT": if ( debug > 0 ) System.out.print("R:");
                                    break;
                        case "FULL" : if ( debug > 0 ) System.out.print("FR:");
                                    break;
                    }
                    System.out.println( JoinData.left_empty + matching_line );
                }
            }

        }
        catch ( FileNotFoundException e ) {
            System.out.println( "----------------------------------------------------------" );
            System.out.println( "Expecting datafile to be named: " + JoinData.large_table );
            System.out.println( e.getMessage() );
            System.exit(-2);
        }
        catch ( IOException e ) {
            System.out.println( "----------------------------------------------------------" );
            System.out.println( "Exception while reading data from file" );
            System.out.println( e.getMessage() );
            System.exit(-3);
        }
        catch ( Exception e ) {
            System.out.println( "----------------------------------------------------------" );
            System.out.println( "An unexpected exception took place" );
            System.out.println( e.getMessage() );
            System.exit(-1);
        }
    
        return;
    }


    public void optionsParser( String[] args ) {
        try {
            File f = new File(args[0]);
            f.isFile();
            Options.put("large_table", args[0]);
        } 
        catch ( Exception e ) {
            Options.put("large_table", "large.csv");
            if ( debug > 0 ) System.out.println( ">>> No large csvfile specified, defaulting to:" + Options.get("large_table"));
        }
        try {
            File f = new File(args[1]);
            f.isFile();
            Options.put("small_table", args[1]);
        } 
        catch ( Exception e ) {
            Options.put("small_table", "small.csv");
            if ( debug > 0 ) System.out.println( ">>> No small csvfile specified, defaulting  to:" + Options.get("small_table"));
        }
        try {
            Integer.parseInt(args[2]);
            Options.put("large_table_fieldnum", args[2]);
        } 
        catch ( Exception e ) {
            Options.put("large_table_fieldnum", "0");
            if ( debug > 0 ) System.out.println( ">>> No column num. to join on first table :" + Options.get("large_table_fieldnum"));
        }
        try {
            Integer.parseInt(args[3]);
            Options.put("small_table_fieldnum", args[3]);
        } 
        catch ( Exception e ) {
            Options.put("small_table_fieldnum", "0");
            if ( debug > 0 ) System.out.println( ">>> No column num. to join on second table: " + Options.get("small_table_fieldnum"));
        }
        try {
            Options.put("jointype", args[4].toUpperCase());
        } 
        catch ( Exception e ) {
            Options.put("jointype", "INNER" );
            if ( debug > 0 ) System.out.println( ">>> No join-type specified for the merge, using: JOIN " + Options.get("jointype"));
        }
        try {
            Options.put("separator", args[5] );
        } 
        catch ( Exception e ) {
            Options.put("separator", "|");
            if ( debug > 0 ) System.out.println( ">>> No separator specified for the merge, using: " + Options.get("separator"));
        }
    }


    public static void main( String[] args ) {
        TableJoiner joiner = new TableJoiner();
        joiner.optionsParser(args);
        joiner.setup();
        joiner.buildIndex();
        if  ( joiner.debug > 0 )
            joiner.printIndex();
        joiner.joinTables();
        System.exit(0);
    }
}

